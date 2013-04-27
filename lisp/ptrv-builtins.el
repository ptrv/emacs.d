;;; ptrv-builtins.el --- emacs builtins conf

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
;; default browser
(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; ido recent file
(setq ido-max-directory-size 100000)

(setq sentence-end-double-space nil)

;; follow version controlled symlinks automatically
(setq vc-follow-symlinks t)

(setq live-disable-zone t)

;; debug messages
(setq debug-on-error nil)

;; scroll compilation buffer
;;(setq compilation-scroll-output 'first-error)
(setq compilation-scroll-output t)

;; disabled commands
(put 'downcase-region 'disabled nil)
(put 'updacase-region 'disabled nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; fix tramp backups
;; (add-to-list 'bkup-backup-directory-info
;;              (list tramp-file-name-regexp ""))
;; (setq tramp-bkup-backup-directory-info bkup-backup-directory-info)

;; ;;disable backups of files edited with tramp
;;(setq tramp-bkup-backup-directory-info nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippie-expand functions list
;; http://trey-jackson.blogspot.de/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert
(auto-insert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-wrapping
(set-default 'fill-column 72)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;remove bells
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'recentf)
(setq recentf-save-file (concat ptrv-tmp-dir "recentf")
      recentf-max-saved-items 200)
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
        (message "Aborting")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;When you visit a file, point goes to the last place where it was
;;when you previously visited. Save file is set to live-tmp-dir/places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat ptrv-tmp-dir "places"))

;;enable cua-mode for rectangular selections
;; (require 'cua-base)
;; (require 'cua-gmrk)
;; (require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))


(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t)

;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)


;;default to unified diffs
(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defun ptrv-cleanup-whitespace ()
  (let ((whitespace-style '(trailing empty)) )
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'ptrv-cleanup-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat ptrv-tmp-dir "savehist"))
(savehist-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-builtins)
;;; ptrv-builtins.el ends here
