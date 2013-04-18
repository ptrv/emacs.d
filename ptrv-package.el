;;; ptrv-package.el --- packae-conf

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

(require 'package)
(custom-set-variables
 '(package-user-dir (concat dotfiles-dir "elpa")))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;;("org" . "http://orgmode.org/elpa/")
                  ))
  (add-to-list 'package-archives source t))

(package-initialize)

(defadvice package-compute-transaction (before package-compute-transaction-reverse
                                               (package-list requirements)
                                               activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ptrv-packages '(magit
                        ;;auctex
                        wrap-region
                        go-mode
                        starter-kit-eshell
                        ack-and-a-half
                        session
                        notify
                        edit-server
                        color-theme
                        ;;org-plus-contrib
                        markdown-mode
                        js2-mode
                        apache-mode
                        yaml-mode
                        xml-rpc
                        pytest
                        git-commit-mode
                        gitignore-mode
                        gitconfig-mode
                        gnuplot
                        dtrt-indent
                        exec-path-from-shell
                        s
                        dash
                        ido-ubiquitous
                        refheap
                        epc
                        iflipb
                        find-file-in-project
                        pomodoro
                        flycheck
                        pandoc-mode
                        iedit
                        glsl-mode
                        ag
                        cmake-mode
                        pylint
                        lua-mode
                        ahg
                        autopair
                        tea-time
                        json-mode
                        iy-go-to-char
                        projectile
                        popwin
                        edit-server-htmlize
                        move-text
                        pcmpl-git
                        multiple-cursors
                        expand-region
                        w3m
                        flymake-cursor
                        litable
                        smex
                        auto-complete
                        yasnippet
                        clojure-mode
                        align-cljlet
                        nrepl
                        gist
                        paredit
                        window-number
                        elisp-slime-nav
                        buffer-move
                        ac-nrepl
                        undo-tree
                        scratch
                        browse-kill-ring
                        ace-jump-mode
                        git-gutter
                        nyan-mode
                        key-chord
                        jedi
                        smooth-scrolling
                        rainbow-delimiters
                        mic-paren
                        nrepl-eval-sexp-fu
                        mwe-log-commands
                        uuid
                        idomenu
                        )
  "A list of packages to ensure are installed at launch.")

(dolist (p ptrv-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'ptrv-package)
;;; ptrv-package.el ends here
