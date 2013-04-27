;;; ptrv-buffer.el --- buffer-conf

;; Copyright (C) 2013  ptrv <mail@petervasil.net>

;; Author: ptrv <mail@petervasil.net>
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

(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default indent-tabs-mode nil) ; And force use of spaces
(setq-default c-basic-offset 4)     ; indents 4 chars
(setq-default tab-width 4)          ; and 4 char wide for TAB

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

; (add-hook 'write-file-hooks
;           (lambda () (if (not indent-tabs-mode)
;                          (untabify (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do not allow to kill the *scratch* buffer
(defvar unkillable-scratch-buffer-erase)
(setq unkillable-scratch-buffer-erase nil)
(defun toggle-unkillable-scratch-buffer-erase ()
  (interactive)
  (if unkillable-scratch-buffer-erase
      (progn
        (setq unkillable-scratch-buffer-erase nil)
        (message "Disable scratch-buffer erase on kill!"))
    (progn
      (setq unkillable-scratch-buffer-erase t)
      (message "Enable scratch-buffer erase on kill!"))))

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (if unkillable-scratch-buffer-erase
            (delete-region (point-min) (point-max)))
        nil
        )
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make file executabable on save if has shebang
(add-hook 'after-save-hook
          #'(lambda ()
              (and (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (save-match-data
                         (looking-at "^#!"))))
                   (not (file-executable-p buffer-file-name))
                   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
                   (message
                    (concat "Saved as script: " buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix whitespace-cleanup
;; http://stackoverflow.com/a/12958498/464831
(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
                                      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-buffer)

;;; ptrv-buffer.el ends here
