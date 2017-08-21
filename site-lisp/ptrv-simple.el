;;; ptrv-simple.el --- ptrv simple functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defun ptrv/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun ptrv/smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (ptrv/smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun ptrv/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
number input."
  (interactive)
  (let ((line-numbers-off-p (if (boundp 'linum-mode)
                                (not linum-mode)
                              t)))
    (unwind-protect
        (progn
          (when line-numbers-off-p
            (linum-mode 1))
          (call-interactively 'goto-line))
      (when line-numbers-off-p
        (linum-mode -1))))
  (save-excursion
    (hs-show-block)))

;;http://www.emacswiki.org/emacs/IncrementNumber
(defun ptrv/change-number-at-point (change)
  "Change number at point with CHANGE fn."
  (save-excursion
    (save-match-data
      (or (looking-at "[0123456789]")
          (error "No number at point"))
      (replace-match
       (number-to-string
        (mod (funcall change (string-to-number (match-string 0))) 10))))))

(defun ptrv/increment-number-at-point ()
  "Increment number at point."
  (interactive)
  (ptrv/change-number-at-point '1+))

(defun ptrv/decrement-number-at-point ()
  "Decrement number at point."
  (interactive)
  (ptrv/change-number-at-point '1-))

(defun ptrv/duplicate-current-line-or-region (arg &optional do-comment)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated.  If DO-COMMENT is non-nil, comment current line or
region."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end))
          (end-orig end) end-comment)
      (when do-comment
        (comment-or-uncomment-region beg end)
        (setq end (line-end-position))
        (setq end-comment end))
      (let ((it 0))
        (while (< it arg)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point))
          (setq it (1+ it))))
      (goto-char (+ origin (* (length region) arg) arg
                    ;; when commenting, advance current point with
                    ;; number of comment characters times marked lines
                    ;; to maintain cursor position
                    (if do-comment
                        (- end-comment end-orig) 0))))))

(defun ptrv/duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated."
  (interactive "p")
  (ptrv/duplicate-current-line-or-region arg t))

;; Don't sort the registers list because it might contain keywords
(defun ptrv/list-registers ()
  "Display a list of nonempty registers saying briefly what they contain."
  (interactive)
  (let ((list (copy-sequence register-alist)))
    ;;(setq list (sort list (lambda (a b) (< (car a) (car b)))))
    (with-output-to-temp-buffer "*Registers*"
      (dolist (elt list)
        (when (get-register (car elt))
          (describe-register-1 (car elt))
          (terpri))))))

(defun ptrv/browse-url ()
  "Open rlf in default browser."
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (if url (browse-url url) (message "No URL at point!"))))

;; define function to shutdown emacs server instance
(defun ptrv/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun ptrv/exit-emacs-client ()
  "Consistent exit emacsclient.

If not in emacs client, echo a message in minibuffer, don't exit
emacs.  If in server mode and editing file, do C-x # server-edit
else do C-x 5 0 delete-frame"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (delete-frame)))

;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun ptrv/comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.

Pass ARG to `comment-dwim'.  If no region is selected and current
line is not blank and we are not at the end of the line, then
comment current line.  Replaces default behaviour of `comment-dwim,'
when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun ptrv/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun ptrv/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ptrv/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (ptrv/indent-buffer)
      (message "Indented buffer."))))

;; http://emacsredux.com/blog/2013/03/28/indent-defun/
(defun ptrv/indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun ptrv/insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(defmacro measure-time (&rest body)
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (message "%s" (- (float-time) ,start)))))

(defun sudo-edit (&optional arg)
  "Edit buffer with superuser privileges.
If ARG is non-nil prompt for filename."
  (interactive "P")
  (let (auth-sources)
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
      (let ((p (point)))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
        (goto-char p)))))

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(defun ptrv/block-comment-c++ (start end)
  (interactive "r")
  (let ((comment-start "/* ")
        (comment-end " */")
        (comment-style 'aligned))
    (comment-region start end)))

(provide 'ptrv-simple)
;;; ptrv-simple.el ends here
