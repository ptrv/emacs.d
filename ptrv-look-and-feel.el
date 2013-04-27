;;; ptrv-lookandfeel.el --- look and feel conf

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

;; Show column numbers in modeline
(setq column-number-mode t)

;; (global-linum-mode t)
;; (setq linum-format "%4d")

;; ;; Redefine linum-on to ignore terminal buffers, because just turning
;; ;; it off in term-mode-hook doesn't work.
;; (setq linum-disabled-modes
;;       '(term-mode slime-repl-mode magit-status-mode help-mode
;;                   eshell-mode erc-mode ibuffer-mode magit-log-edit-mode))
;; (defun linum-on ()
;;   (unless (or (minibufferp) (member major-mode linum-disabled-modes))
;;     (linum-mode 1)))

(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ptrv-themes-dir (concat dotfiles-dir "themes"))
(add-to-list 'load-path ptrv-themes-dir)
(autoload 'color-theme-gandalf-ptrv "gandalf-ptrv" nil nil)
(color-theme-gandalf-ptrv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from colour-pack
(require 'live-fontify-hex)

(font-lock-add-keywords 'lisp-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'emacs-lisp-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'lisp-interaction-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'css-mode
                        '((live-fontify-hex-colors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nyan-mode
(autoload 'nyan-mode "nyan-mode" nil t)
(setq nyan-bar-length 16)
(nyan-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-look-and-feel)

;;; ptrv-lookandfeel.el ends here
