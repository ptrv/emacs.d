;;; copyright 2010 Justin Smith <noisesmith@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
;;; USA

;; this is an optional extension to scel, in order to use it, just add
;; (require 'ext-scel) to your ~/.emacs and make sure that this file is
;; found somewhere in your load-path

(require 'sclang)

(defvar sclang-reply-string nil)
;; redefinition of the scel version, only different on a couple of lines
(defun sclang-process-filter (process string)
  (let ((buffer (get-buffer sclang-post-buffer)))
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
    (sclang-perform-command-no-result 'handleCallback
				      sclang-callback-stack-counter string))

(defcustom sclang-minibuf-results t
  "If not-nil, echo the results of sclang calculations in the minibuffer."
  :group 'sclang-interface
  :type 'boolean)

(defcustom sclang-reply-hook
  '(sclang-error-filter
    sclang-library-load-filter
    sclang-jack-message-filter
    sclang-apply-any-hooks
    sclang-display-results)
  "A list of functions applied to the string returned by an sclang command.
Argument is the post-buffer.
For reading or modifying the string from sclang, use the variable sclang-reply-string"
  :options '((sclang-error-filter
	      sclang-library-load-filter
 	      sclang-jack-message-filter
 	      sclang-apply-any-hooks
 	      sclang-display-results)
	     (sclang-display-results))
   :group 'sclang-interface)

(defun sclang-display-results (buffer)
  (if (and sclang-minibuf-results (> (length sclang-reply-string) 0))
      (sclang-message  (sclang-minibuf-prepare-string sclang-reply-string 72)))
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert sclang-reply-string)))

(defun sclang-apply-any-hooks (buffer)
  (setq sclang-reply-string
	(sclang-apply-any-hooks-rec sclang-reply-string "")))

(defvar sclang-hook-partial-result "")

(defun sclang-apply-any-hooks-rec (string result)
  (let ((string (format "%s%s" sclang-hook-partial-result string))
	this-start this-end this-match next-result next-start key)
    (if (not (string-match "scel_emacs_callback_start " string))
	string
      (setq this-start (match-beginning 0))
      (if (not (string-match " scel_emacs_callback_end\n"
			     (substring string this-start)))
	  (progn (setq sclang-hook-partial-result string)
		 "")
	(setq sclang-hook-partial-result "")
	(setq this-end (+ (match-beginning 0) 1) next-start (match-end 0))
	(setq this-match (substring string this-start this-end))
	(unless (string-match "\\([0-9]+\\) \\(.*\\)" this-match)
	  (error
	   "Malformatted callback call in sclang-apply-any-hooks-rec %S"
	   this-match))
	(setq key (read (match-string 1 this-match)))
	(if (not key)
	    (error
	     "key for callback not found in sclang-apply-any-hooks-rec %S"
	     (match-string 1 this-match))
	  (setq result (format "%s%s%s"
			     result
			     (substring string 0 this-start)
			     (funcall (cadr (assoc key sclang-callback-stack))
				      (match-string 2 this-match))))
	(setq sclang-callback-stack (assq-delete-all key sclang-callback-stack))
	(sclang-apply-any-hooks-rec (substring string next-start) result))))))

(defcustom sclang-collapse t
  "If non-nil, collapse some messages to a single expandable widget"
  :group 'sclang-interface
  :type 'boolean)

(defcustom sclang-error-props '(face (foreground-color . "RGB:ff/0/0"))
  "Text properties to be applied to sclang related error text."
  :group 'sclang-interface
;;  :options '((face ((foreground-color . "red")
;;		    (background-color . "dark gray")))
;;	     (face (foreground-color . "RGB:fff/800/800")))
  :type 'plist)

(defcustom sclang-trunc-props '(face (foreground-color . "RGB:ff/80/ff"))
  "Text properties to be applied to a truncated excerpt."
  :group 'sclang-interface
;;  :options '((face (foreground-color . "violet")))
  :type 'plist)

(defcustom sclang-message-props '(face (foreground-color . "RGB:0/ff/0"))
  "Text properties to be applied to sclang informational messages."
  :group 'sclang-interface
;;  :options '((face (foreground-color . "green")))
  :type 'plist)

(defun sclang-error-filter (buffer)
  "Highlight and make collapsible widgets out of errors."
  (when (string-match "^ERROR: \\(.*\\)" sclang-reply-string)
    (let ((error-string (substring sclang-reply-string (nth 2 (match-data))
				   (nth 3 (match-data))))
	  error-code
	  custom-message)
      (setq error-code (let ((code-start
			      (if (string-match "var code = \\(.*\\)"
						   sclang-reply-string)
				     (nth 2 (match-data)) nil))
			     (code-end
		;;; double check the following regexp, make a better one?
			      (if (string-match "\n.*var doc ="
						sclang-reply-string)
				     (car (match-data)) nil)))
			 (if (and code-start code-end)
			     (sclang-remove-surrounding-spaces
			      (substring sclang-reply-string
					 code-start code-end))
			   "not found")))
      (save-excursion
	(goto-char (point-max))
	(setq custom-message
	      (concat
	       (apply #'propertize (cons "ERROR: " sclang-error-props))
	       (apply #'propertize (cons error-string sclang-message-props))
	       (apply #'propertize (cons " code: " sclang-error-props))
	       (apply #'propertize (cons error-code sclang-trunc-props))))
	(insert custom-message)
	(when sclang-minibuf-results
	  (sclang-message (sclang-minibuf-prepare-string custom-message 72)))
	(when sclang-collapse
	  (sclang-insert-collapsible sclang-reply-string)
	  (setq sclang-reply-string ""))))))

(defvar sclang-loading-libs-state nil)
(defun sclang-library-load-filter (buffer)
  "Highlight and make collapsible widgets out of library load messages."
  (when (string-match "^\tNumPrimitives =" sclang-reply-string)
    (save-excursion
      (goto-char (point-max))
      (insert "Welcome to scel. Type ctrl-c ctrl-h for help.\n")
      (when sclang-collapse
	(insert "Click on the [+] strings below for more detailed info.\n")))
    (setq sclang-loading-libs-state t))
  (when (string-match "^Emacs: Built symbol" sclang-reply-string)
    (setq sclang-loading-libs-state nil))
  (when (and sclang-collapse sclang-loading-libs-state)
    (save-excursion
      (goto-char (point-max))
      (insert  (apply #'propertize (cons "sclang init " sclang-message-props)))
      (insert (apply #'propertize
		     (cons (sclang-minibuf-prepare-string
			     sclang-reply-string 64)
			   sclang-trunc-props)))
      (sclang-insert-collapsible sclang-reply-string))
    (setq sclang-reply-string "")))

(defun sclang-jack-message-filter (buffer)
  "Highlight and make collapsible widgets out of the Jack messages."
  (when	(and sclang-collapse (string-match "^JackDriver: " sclang-reply-string))
    (save-excursion
      (goto-char (point-max))
      (insert (apply #'propertize (cons "JACK: "
					sclang-message-props)))
      (insert (apply #'propertize (cons (sclang-minibuf-prepare-string
					 sclang-reply-string 70)
					sclang-trunc-props)))
      (sclang-insert-collapsible sclang-reply-string)
      (setq sclang-reply-string ""))))

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
    (goto-char (point-max))
    (insert " ")
    (let ((button (insert-button (propertize "[+]" 'sclang-button t))))
      (insert "\n")
      (insert (propertize text 'invisible t 'sclang-collapse t))
      (button-put
       button 'action
       (lambda (but)
	 (save-excursion
	   (let (new-str)
	     (unless (eq (following-char) ?\[)
	       (search-backward "["))
	     (forward-char)
	     (setq new-str
		   (if (eq (following-char) ?-)
		       "+" "-"))
	     (delete-char 1)
	     (insert new-str) ;; surrounding properties are added
	     (let* ((start
		     (next-single-property-change (point) 'sclang-collapse))
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

(defun sclang-minibuf-prepare-string (string width)
  (let ((message-text
	 (sclang-remove-surrounding-spaces
	  (replace-regexp-in-string
	   "[ \t\n]+" " " string))))
    (if (> (length message-text) width)
	(format "%s…%s"
		(substring message-text 0 (/ (- width 1) 2))
		(substring message-text (/ (- width 1) -2)))
      message-text)))

(defun sclang-remove-surrounding-spaces (string)
  (replace-regexp-in-string
   "\\(^[ \t\n]+\\)\\|\\([ \t\n]+$\\)" "" string))

(defun sclang-get-method-args (method class function)
  (let ((get-args-format
	 (concat
	  "var args, defaults;"
	  "var classSymbol = \\%s;"
	  "var methodSymbol = \\%s;"
	  "var method;"
	  "if( classSymbol.asClass.notNil, {"
	  "  method=classSymbol.asClass.findRespondingMethodFor(methodSymbol);"
	  "  if( method.isNil&&(\"Meta_\"++classSymbol.asString)"
	  "    .asSymbol.asClass.notNil, {"
	  "      method = (\"Meta_\"++classSymbol.asString).asSymbol"
	  "        .asClass.findRespondingMethodFor(methodSymbol)"
	  "  } );"
	  "  if( method.notNil, {"
	  "    args = method.argNames[1..].asArray;"
	  "    defaults = method.prototypeFrame[1..].asArray;"
	  "    args.collect{|a, i|a.asString++if(defaults[i].notNil,"
	  "      {\"=\"++(defaults[i]).asString}, \"\")}"
	  "  }, [\"\"])}, [\"\"])")))
    (sclang-eval-string-with-hook
     (format get-args-format class method)
     `(lambda (method-args)
	(apply #',function (list (read method-args)))))))


(defun sclang-with-method-args (method function &optional class)
  (let ((get-classes-string
	 (format
	  (concat "var di=[];Class.allClasses.do{|c|if(c.respondsTo(\\%s)"
		  ",{di=di.add(c.asString)})};di")
	  method)))
    (if (not class)
	(sclang-eval-string-with-hook
	 get-classes-string
	 `(lambda (classes)
	   (let ((chosen-class
		  (completing-read
		   (format "Class implementing %s: " ,method)
		   (read classes))))
	     (when (> (length chosen-class) 0)
	       (sclang-get-method-args ,method chosen-class #',function)))))
      (sclang-get-method-args method class function))))

(defun sclang-insert-method-args (&optional method class)
  (interactive)
  (save-excursion
    (setq method (sclang-symbol-at-point))
    (while (not (or method (= (point) 1)))
      (backward-char)
      (setq method (sclang-symbol-at-point)))
    (forward-word)
    (let ((buff (current-buffer))
	  (pt (point))
	  classname)
      (backward-char)
      (search-backward ".")
      (backward-char)
      (let ((case-fold-search nil))
	(when (and (sclang-symbol-at-point)
		   (string-match "^[A-Z][a-z_A-Z0-9]+$"
				 (sclang-symbol-at-point)))
	  (setq class (sclang-symbol-at-point))))
      (sclang-with-method-args
       method
       `(lambda (method-args)
	  (with-current-buffer ,buff
	    (save-excursion
	      (goto-char ,pt)
	      (insert "( ")
	      (while method-args
		(insert (car method-args))
		(setq method-args (cdr method-args))
		(when method-args
		  (insert ", ")))
	      (insert " )"))))
       class))))

(defun sclang-echo-method-args (&optional method)
  (interactive)
  (save-excursion
    (setq method (sclang-symbol-at-point))
    (while (not (or method (= (point) 1)))
      (backward-char)
      (setq method (sclang-symbol-at-point)))
    (forward-word)
    (sclang-with-method-args
     method
     (lambda (method-args)
       (message (format "%s" method-args))))))

(provide 'ext-scel)
