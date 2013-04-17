;;; ptrv-misc.el --- misc conf

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; session.el
;; save a list of open files in $HOME/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse-kill-ring
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-no-duplicates t)
(setq browse-kill-ring-display-duplicates nil)
(setq browse-kill-ring-highlight-inserted-item nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-time
(require 'insert-time)
(setq insert-date-format "%Y-%m-%d")
(setq insert-time-format "%H:%M:%S")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pomodoro.el
(autoload 'pomodoro-add-to-mode-line "pomodoro" t)
(pomodoro-add-to-mode-line)
(setq pomodoro-sound-player "/usr/bin/aplay")
(setq pomodoro-break-start-sound (concat ptrv-etc-dir "sounds/alarm.wav"))
(setq pomodoro-work-start-sound (concat ptrv-etc-dir "sounds/alarm.wav"))

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
;; switch between sqlite3 and spatialite excutable
(defun sql-switch-spatialite-sqlite ()
  (interactive)
  (let* ((sqlprog sql-sqlite-program)
         (change (if (string-match "sqlite" sqlprog)
                     (executable-find "spatialite")
                   (executable-find "sqlite3"))))
    (setq sql-sqlite-program change)
    (message "sql-sqlite-program changed to %s" change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the silver searcher
(setq ag-highlight-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pure-mode
(autoload 'pure-mode "pure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pure\\(rc\\)?$" . pure-mode))
(eval-after-load "pure-mode"
  '(progn
     (add-hook 'pure-mode-hook 'hs-minor-mode)
     (add-hook 'pure-eval-mode-hook
               (lambda ()
                 (define-key pure-eval-mode-map [up] 'comint-previous-input)
                 (define-key pure-eval-mode-map [down] 'comint-next-input)))
     (define-key pure-mode-map (kbd "C-c M-p") 'run-pure)
     (define-key pure-mode-map (kbd "C-x M-p") 'pure-scratchpad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda-mode
(autoload 'lambda-mode "lambda-mode" nil t)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-operator
(autoload 'smart-operator-mode "smart-operator" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tea-time
(autoload 'tea-time "tea-time" nil t)
(setq tea-time-sound (concat ptrv-etc-dir "sounds/alarm.wav"))
(cond
 ((eq system-type 'darwin)
  (setq tea-time-sound-command "afplay %s"))
 ((eq system-type 'gnu/linux)
  (setq tea-time-sound-command "aplay %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xah lee modes
(autoload 'xmsi-mode "xmsi-math-symbols-input"
  "Load xmsi minor mode for inputting math/Unicode symbols." t)
(eval-after-load "xmsi-math-symbols-input"
  '(progn
     (define-key xmsi-keymap (kbd "S-SPC") 'nil)
     (define-key xmsi-keymap (kbd "C-c C-8") 'xmsi-change-to-symbol)))


;; xub-mode
(autoload 'xub-mode "xub-mode" "Load xub-mode for browsing Unicode." t)
(defalias 'unicode-browser 'xub-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display visited file's path as frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iflipb-conf.el

(setq iflipb-ignore-buffers '("*Ack-and-a-half*"
                              "*Help*"
                              "*Compile-Log*"
                              "*Ibuffer*"
                              "*Messages*"
                              "*scratch*"
                              "*Completions*"
                              "*magit"
                              "*Pymacs*"
                              "*clang-complete*"
                              "*compilation*"
                              "*Packages*"
                              "TAGS"
                              "*file-index*"
                              " output*"
                              "*tramp/"
                              "*project-status*"
                              "SCLang:PostBuffer*"
                              ))

(setq iflipb-wrap-around t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edit-server

(autoload 'edit-server-start "edit-server" "" t)

(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)

(edit-server-start)

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . gfm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iedit
(setq iedit-toggle-key-default (kbd "C-;"))
(define-key global-map iedit-toggle-key-default 'iedit-mode)
(define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
(define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
(define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-misc)
;;; ptrv-misc.el ends here
