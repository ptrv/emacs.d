(defvar sclang-reply-string nil)
;; redefinition of the scel version, only different on a couple of lines
(defun sclang-process-filter (process string)
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (when (and (> sclang-max-post-buffer-size 0)
		 (> (buffer-size) sclang-max-post-buffer-size))
	(erase-buffer))
      (let ((move-point (or sclang-auto-scroll-post-buffer
			    (= (point) (process-mark process)))))
	(save-excursion
	  ;; replace mac-roman bullet with unicode character
	  ;; (subst-char-in-string sclang-bullet-latin-1 sclang-bullet-utf-8 string t)
	  (setq sclang-reply-string string)
	  ;; insert the text, advancing the process marker.
	  (goto-char (process-mark process))
	  ;;; added hook here for catching callbacks
	  ;; (insert string)
	  (run-hook-with-args 'sclang-reply-hook buffer)
	  (set-marker (process-mark process) (point)))
	(when move-point
	  (goto-char (process-mark process))
	  (walk-windows
	   (lambda (window)
	     (when (eq buffer (window-buffer window))
	       (set-window-point window (process-mark process))))
	   nil t))))))

(defvar sclang-callback-stack '())
(defvar sclang-callback-stack-counter 0)

(defun sclang-eval-string-with-hook (string lambda)
  "Send STRING to the sclang process for evaluation. Callback lambda will be
called on the string received back from sclang. Return STRING if successful,
otherwise nil. Return value of the lambda will be printed in the postbuffer"
    (add-to-list 'sclang-callback-stack (cons
					 (incf sclang-callback-stack-counter)
					 (list lambda)))
    (sclang-send-string
     sclang-token-interpret-print-cmd-line
     (format "[\\scel_emacs_callback, %s, %s]"
	     sclang-callback-stack-counter string)))

(defvar sclang-reply-hook)
(setq sclang-reply-hook
  ;; a list of functions to be applied to the string returned by an
  ;; sclang command
  ;; todo: generalize this first callback, make a version that is a long
  ;; term responder, not self deleting on first call
  '((lambda (buffer) (sclang-error-filter buffer))
    (lambda (buffer)
      (when (and (> (length sclang-reply-string) 23)
		    (string= (substring sclang-reply-string 0 24)
		     "\n[ scel_emacs_callback, "))
	(with-current-buffer buffer
	  (let* ((reply (substring sclang-reply-string 24))
		 (key-start (string-match "[0-9]" reply))
		 (key-end (string-match "[^0-9]" (substring reply key-start)))
		 (key (read (substring reply key-start key-end)))
		 (response (substring reply (+ key-end 2) -3))
		 (response-string (format "%s%s" response
					  (substring reply -1))))
	    (setq sclang-reply-string
		  (funcall (cadr (assoc key sclang-callback-stack))
			   response-string))
	    (setq sclang-callback-stack
		  (assq-delete-all key sclang-callback-stack))))))
    (lambda (buffer)
      (with-current-buffer buffer
	(end-of-buffer)
	(insert sclang-reply-string)))))

(defvar sclang-collapse-errors t)

(defun sclang-error-filter (buffer)
  "highlight and make collapsible widgets out of errors"
  (when (string-match "^ERROR: \\(.*\\)" sclang-reply-string)
    (let ((error-string (substring sclang-reply-string (nth 2 (match-data))
				   (nth 3 (match-data))))
	  (error-code)) ;; double check the following regexp, make a better one?
      (setq error-code (let ((code-start
			      (if (string-match "var code = \\(.*\\)"
						   sclang-reply-string)
				     (nth 2 (match-data)) nil))
			     (code-end
			      (if (string-match "
.*var doc ="
						sclang-reply-string)
				     (car (match-data)) nil)))
			 (if (and code-start code-end)
			     (substring sclang-reply-string
					code-start code-end)
			   "not found")))
      (save-excursion
	(end-of-buffer)
	(insert (format (propertize "ERROR: %s code: %s"
				    'face '((foreground-color . "red")
					    (background-color . "black")))
			(propertize error-string
				    'face '((foreground-color . "green")
					    (background-color . "black")))
			(propertize error-code
				    'face '((foreground-color . "violet")
					    (background-color . "black")))))
	(when sclang-collapse-errors
	  (sclang-insert-collapsible sclang-reply-string)
	  (setq sclang-reply-string ""))))))

(defun sclang-eval-face (string props)
  "Execute the region as SuperCollider code, and print the result in
a specific color"
  (sclang-eval-string-with-hook 
   string
   `(lambda (str)
      (apply #'propertize (cons str ',props)))))


(defun sclang-eval-red-on-black (start end)
  "Execute the region as SuperCollider code, and print the result in red on a
black background"
  (interactive "r")
  (sclang-eval-face
   (buffer-substring-no-properties start end)
   '(face ((foreground-color . "red")
	   (background-color . "black")))))

(defun sclang-minibuf-display (format string)
  (sclang-eval-string-with-hook
   string
   `(lambda (str)
      (funcall #'message (format ,format str))
      "")))

(defun sclang-minibuf-region (start end)
  (interactive "r")
  (sclang-minibuf-display "from sclang: %s"
			  (buffer-substring-no-properties start end)))

(defun sclang-insert-collapsible (text)
  (save-excursion 
    (end-of-buffer)
    (insert " ")
    (let ((button (insert-button (propertize "[+]" 'sclang-button t))))
      (insert "\n")
      (insert (propertize text 'invisible t 'sclang-collapse t))
      (insert "\n")
      (button-put
       button 'action
       (lambda (but)
	 (save-excursion
	   (let (new-str)
	     (goto-char pos)
	     (unless (eq (following-char) ?\[)
	       (search-backward "["))
	     (forward-char)
	     (setq new-str
		   (if (eq (following-char) ?-)
		       "+" "-"))
	     (delete-char 1)
	     (insert-string new-str) ;; surrounding properties are added
	     (let* ((start
		     (next-single-property-change pos 'sclang-collapse))
		    (end (or
			  (next-single-property-change start
						       'sclang-collapse)
			  (point-max)))
		    (new-invis-val
		     (if (text-property-any start end 'invisible t)
			 nil t)))
	       (add-text-properties
		start end
		(list 'invisible new-invis-val))))))))))
