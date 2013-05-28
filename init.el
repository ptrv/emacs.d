;;; init.el --- ptrv init

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

(setq initial-scratch-message ";;
;; I'm sorry, Emacs failed to start correctly.
;; Hopefully the issue will be simple to resolve.
;;
;;                _.-^^---....,,--
;;            _--                  --_
;;           <          SONIC         >)
;;           |       BOOOOOOOOM!       |
;;            \._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
")

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get hostname
(setq ptrv-hostname (replace-regexp-in-string
                     "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)"
                     ""
                     (with-output-to-string
                       (call-process "hostname" nil standard-output))))
(setq system-name ptrv-hostname)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set all dirs
(setq
 ptrv-etc-dir      (file-name-as-directory (concat dotfiles-dir "etc"))
 ptrv-tmp-dir      (file-name-as-directory (concat dotfiles-dir "tmp"))
 ptrv-autosaves-dir(file-name-as-directory (concat ptrv-tmp-dir "autosaves"))
 ptrv-backups-dir  (file-name-as-directory (concat ptrv-tmp-dir "backups"))
 ptrv-pscratch-dir (file-name-as-directory (concat ptrv-tmp-dir "pscratch"))
 )

;; create tmp dirs if necessary
(make-directory ptrv-tmp-dir t)
(make-directory ptrv-autosaves-dir t)
(make-directory ptrv-backups-dir t)
(make-directory ptrv-pscratch-dir t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist
    (project (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (and (file-directory-p project)
             (not (string-match "_extras" project)))
    (add-to-list 'load-path project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(load "~/.emacs-locals.el" 'noerror)
(require 'my-secrets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; essential macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defmacro with-library (symbol &rest body)
  "Evaluate BODY only if require SYMBOL is successful."
  (declare (indent defun))
  `(when (require ,symbol nil t)
     ,@body))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ptrv-pkgs
      '(ptrv-backup
        ptrv-package
        ptrv-builtins
        ptrv-defuns
        ptrv-look-and-feel
        ptrv-window-number
        ptrv-ido
        ptrv-shell
        ptrv-lisp
        ptrv-clojure
        ptrv-complete
        ptrv-tramp
        ptrv-ibuffer
        ptrv-vcs
        ptrv-nrepl
        ptrv-yasnippet
        ptrv-misc
        ptrv-project
        ptrv-webjump
        ptrv-popwin
        ptrv-buffer
        ptrv-org
        ptrv-latex
        ptrv-filetypes
        ptrv-abbrev
        ptrv-pandoc
        ptrv-golang
        ptrv-xml
        ptrv-erc
        ptrv-dsp
        ptrv-editing
        ptrv-markdown
        ptrv-processing
        ptrv-flycheck
        ptrv-flymake
        ptrv-hideshow
        ptrv-bindings
        ptrv-autopair
        ptrv-clean-mode-line
        ))

(cond
 ((eq system-type 'darwin)
  (setq ptrv-pkgs (append ptrv-pkgs '(ptrv-osx))))
 ((eq system-type 'gnu/linux)
  (setq ptrv-pkgs
        (append ptrv-pkgs '(ptrv-supercollider
                            ptrv-elpy
                            ptrv-python
                            ptrv-c
                            ptrv-linux
                            )))))

(add-to-list 'load-path (concat dotfiles-dir "modules"))
(dolist (file ptrv-pkgs)
  (require file))

(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

(Xlaunch (require 'ptrv-x11) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom settings
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; welcome-message stuff
(setq ptrv-welcome-messages
      (if (ptrv-user-first-name-p)
          (list (concat "Hello " (ptrv-user-first-name) ", somewhere in the world the sun is shining for you right now.")
                (concat "Hello " (ptrv-user-first-name) ", it's lovely to see you again. I do hope that you're well.")
                (concat (ptrv-user-first-name) ", turn your head towards the sun and the shadows will fall behind you.")
                )
        (list  "Hello, somewhere in the world the sun is shining for you right now."
               "Hello, it's lovely to see you again. I do hope that you're well."
               "Turn your head towards the sun and the shadows will fall behind you.")))

(defun ptrv-welcome-message ()
  (nth (random (length ptrv-welcome-messages)) ptrv-welcome-messages))

(setq initial-scratch-message (concat ";;
;; Emacs on " system-name " [" (symbol-name system-type) "]
;;
;; " (ptrv-welcome-message) "

"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el ends here
