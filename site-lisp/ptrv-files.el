;;; ptrv-files.el --- ptrv files configuration       -*- lexical-binding: t; -*-

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

(defvar recentf-list)

(defun ptrv/ido-recentf-open ()
  "Find a recent file using ido."
  (interactive)
  (unless (bound-and-true-p recentf-mode)
    (user-error "Recentf Mode disabled"))
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun ptrv/get-standard-open-command ()
  "Get the standard command to open a file."
  (cond
   ((eq system-type 'darwin) "open")
   ((eq system-type 'gnu/linux) "xdg-open")))

;; http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun ptrv/open-with (arg)
  "Open the file visited by the current buffer externally.

Use the standard program to open the file.  With prefix ARG,
prompt for the command to use."
  (interactive "P")
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (let ((marked-files (dired-get-marked-files)))
                          (if marked-files
                              marked-files
                            (directory-file-name (dired-current-directory))))
                      (list (buffer-file-name))))
         (doIt (if (<= (length file-list) 5)
                   t
                 (y-or-n-p "Open more than 5 files? "))))
    (when doIt
      (unless (car file-list)
        (user-error "This buffer is not visiting a file"))
      (let ((command (unless arg (ptrv/get-standard-open-command))))
        (unless command
          (setq command (read-shell-command "Open current file with: ")))
        (let ((open-fn (lambda (file-path)
                         (cond
                          ((eq system-type 'gnu/linux)
                           (let ((process-connection-type nil))
                             (start-process "" nil command (shell-quote-argument file-path))))
                          ((eq system-type 'darwin)
                           (shell-command
                            (concat command " " (shell-quote-argument file-path))))))))
          (mapc open-fn file-list))))))

(defun ptrv/launch-directory ()
  "Open parent directory in external file manager."
  (interactive)
  (let ((command (ptrv/get-standard-open-command))
        (dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
                (expand-file-name default-directory))))
    (cond
     ((eq system-type 'gnu/linux)
      (let ((process-connection-type nil))
        (start-process "" nil command dir)))
     ((eq system-type 'darwin)
      (shell-command (concat command " " dir))))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun ptrv/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun ptrv/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ptrv/delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (when (y-or-n-p "Delete file and its buffer? ")
    (let ((filename (buffer-file-name)))
      (cond
       ((not filename) (kill-buffer))
       ((vc-backend filename) (vc-delete-file filename))
       (:else
        (delete-file filename)
        (kill-buffer))))))

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun ptrv/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun ptrv/byte-recompile-site-lisp ()
  "Recompile user site-lisp directory."
  (interactive)
  (dolist (project (directory-files
                    (locate-user-emacs-file "site-lisp") t "^[^_]\\w+"))
    (when (file-directory-p project)
      (byte-recompile-directory project 0))))

(defun ptrv/byte-recompile-elpa ()
  "Recompile elpa directory."
  (interactive)
  (when (boundp 'package-user-dir)
    (byte-recompile-directory package-user-dir 0)))

(defun ptrv/byte-recompile-init ()
  "Recompile user's init file."
  (interactive)
  (byte-recompile-file (locate-user-emacs-file "init.el") t 0))

(defun ptrv/byte-recompile-home ()
  "Recompile all relevant files in user's Emacs dir."
  (interactive)
  (ptrv/byte-recompile-site-lisp)
  (ptrv/byte-recompile-elpa)
  (ptrv/byte-recompile-init))

(defun ptrv/refresh-file ()
  "Revert file in current buffer."
  (interactive)
  (revert-buffer nil t)
  (message "Buffer reverted!"))

(provide 'ptrv-files)
;;; ptrv-files.el ends here
