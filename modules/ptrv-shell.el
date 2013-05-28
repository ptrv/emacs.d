;;; ptrv-shell.el --- shell-conf

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

(exec-path-from-shell-initialize)

;;; Eshell
(after 'eshell
  (message "Eshell config has been loaded !!!")
  ;; (eval-when-compile (require 'eshell nil t))
  (setq eshell-aliases-file (concat dotfiles-dir "etc/eshell_aliases"))

  (defun eshell/clear ()
    "04Dec2001 - sailor, to clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun eshell/e (file)
    (find-file-other-window file))

  (add-hook 'eshell-prompt-load-hook
            (lambda ()
              (set-face-attribute 'eshell-prompt-face nil :foreground "dark green")))

  (autoload 'pcomplete/go "pcmpl-go" nil nil)
  (autoload 'pcomplete/lein "pcmpl-lein" nil nil)

  (after 'auto-complete
    (require 'eshell-ac-pcomplete)))

;;; Term
(setq term-default-bg-color "black")
(setq term-default-fg-color "white")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'ptrv-shell)
;;; ptrv-shell.el ends here
