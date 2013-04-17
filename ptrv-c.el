;;; ptrv-c.el --- c-conf

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

(setq c-default-style "linux")

;; Hook auto-complete into clang
(eval-after-load "cc-mode"
  '(progn
     (require 'auto-complete-clang-async)
     (setq ac-clang-complete-executable
           (concat dotfiles-dir "site-lisp/emacs-clang-complete-async/clang-complete"))
     (when (not (file-exists-p ac-clang-complete-executable))
       (warn (concat "The clang-complete executable doesn't exist")))
     ;; (when (not (file-exists-p clang-complete-executable))
     ;;   (warn (concat "The clang-complete executable doesn't exist - please run "
     ;;                 dotfiles-dir "setup.sh to compile it.")))
     ;; Add Qt4 includes to load path if installed
     (when (file-exists-p "/usr/include/qt4")
       (setq ac-clang-flags
             (mapcar (lambda (f) (concat "-I" f))
                     (directory-files "/usr/include/qt4" t "Qt\\w+"))))
     ))

(add-hook 'c++-mode-hook
          (lambda ()
            (unless (string-match ".*flycheck.*" buffer-file-name)
              (setq ac-sources '(ac-source-clang-async))
              (ac-clang-launch-completion-process))
            (dtrt-indent-mode 1)
            (set (make-local-variable 'before-save-hook) nil)))

(defun set-ff-find-other-file-binding ()
  (local-set-key  (kbd "C-c o") 'ff-find-other-file))
(add-hook 'c-mode-hook 'set-ff-find-other-file-binding)
(add-hook 'c++-mode-hook 'set-ff-find-other-file-binding)

;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq clang-complete-executable "~/.live-packs/ptrv-pack/lib/clang-complete-async/clang-complete")
;;   (when (not (file-exists-p clang-complete-executable))
;;     (warn (concat "The clang-complete executable doesn't exist")))
;;   (setq ac-sources '(ac-source-clang-async))
;;   (launch-completion-proc)
;; )

;; (defun my-ac-config ()
;;   ;;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'c-mode-hook 'ac-cc-mode-setup)
;;   (add-hook 'c++-mode-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-c)
;;; ptrv-c.el ends here
