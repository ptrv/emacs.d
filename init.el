;;; init.el --- Emacs configuration of Peter Vasil
;;
;; Copyright (c) 2013-2014, 2016 Peter Vasil <mail@petervasil.net>
;;
;; Author: Peter Vasil <mail@petervasil.net>
;; URL: https://gihub.com/ptrv/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Peter Vasil.

;;; Code:

;;; Debugging
(setq message-log-max 10000)

(when (version-list-< (version-to-list emacs-version) '(24 4))
  (error "This configuration needs Emacs 24.4, but this is %s!" emacs-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * basic init stuff
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

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-cocoa-emacs* (and *is-mac* (eq window-system 'ns)))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-x11* (eq window-system 'x))
(defconst *is-windows* (eq system-type 'windows-nt))

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
(defvar ptrv/hostname (car (process-lines "hostname")))
(setq system-name ptrv/hostname)

;; set all dirs
(defvar ptrv/etc-dir      (file-name-as-directory (locate-user-emacs-file "etc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * backup
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup"))))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * package
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t)
(require 'diminish)
(require 'bind-key)

(bind-key "C-c h b" #'describe-personal-keybindings)

(use-package package-build
  :disabled t
  :load-path "~/src/melpa"
  :commands (package-build-archive
             package-build-create-recipe
             package-build-current-recipe)
  :config
  (defun ptrv/package-build-dump-archive-contents ()
    (interactive)
    (package-build-dump-archive-contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * tls
(defcustom ptrv/trustfile-command nil
  "Command to retrieve trustfile path.

Something like: `python -m certifi'."
  :type 'string)

;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package tls
  :config
  (if ptrv/trustfile-command
      (let ((trustfile
             (replace-regexp-in-string
              "\\\\" "/"
              (replace-regexp-in-string
               "\n" ""
               (shell-command-to-string ptrv/trustfile-command)))))
        (setq tls-program
              (list
               (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                       (if (eq window-system 'w32) ".exe" "") trustfile)))
        (use-package gnutls
          :config (setq gnutls-verify-error t
                        gnutls-trustfiles (list trustfile))))
    (warn "Cannot set tls program because, `ptrv/trustfile-command' is nil")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * paradox
(use-package paradox
  :ensure t
  :bind (("C-c l p" . paradox-list-packages))
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * PATH
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * custom settings
(defconst ptrv/custom-file (locate-user-emacs-file "custom.el"))
(use-package cus-edit
  :init (load ptrv/custom-file :no-error :no-message)
  :config (setq custom-file ptrv/custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-replacement-alist
        (append which-key-replacement-alist
                '(;; Prettify hydra entry points
                  ((nil . "/body\\'") . (nil . "|="))
                  ;; Drop my personal prefix
                  ((nil . "\\`ptrv.*?/") . (nil . ""))
                  ((nil . "\\`cscope-") . (nil . ""))
                  ((nil . "\\`helm-cscope-") . (nil  . ""))
                  ((nil . "\\`ycmd-") . (nil . "")))))
  (which-key-add-key-based-replacements
    "C-c a" "applications"
    "C-c D" "diff"
    "C-c g" "git"
    "C-c g g" "github/gists"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c f b" "byte-compilation"
    "C-c b" "buffers"
    "C-c p" "projects"
    "C-c i" "insert"
    "C-c h" "help"
    "C-c j" "jump"
    "C-c l" "list"
    "C-c t" "toggle"
    "C-c x" "text"
    "C-c x a" "align"
    "C-c ;" "comment"
    "C-c u" "cursors"
    "C-c C-p" "sql"
    "C-c s" "search"
    "C-c o" "org"
    "C-c m" "major-mode"
    "C-c w" "windows"
    "C-c /" "google-this"
    "C-c !" "flycheck"
    "C-c ," "cscope"
    "C-c C-y" "ycmd"
    "C-." "run")

  (which-key-add-major-mode-key-based-replacements 'emacsu-lisp-mode
    "C-c m" "elisp/personal")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  :diminish which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * hydra
(use-package hydra
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * builtins
(setq-default fill-column 80
              indent-tabs-mode nil ; And force use of spaces
              c-basic-offset 4     ; indents 4 chars
              tab-width 4)         ; and 4 char wide for TAB

(use-package bookmark
  :bind ("C-c l b" . list-bookmarks)
  :config (setq bookmark-save-flag t))

(use-package simple
  :bind (("C-m" . newline-and-indent)
         ("M-j" . ptrv/join-line)
         ("M-g n" . ptrv-errors/next-error)
         ("M-g p" . ptrv-errors/previous-error))
  :init
  (defun ptrv/join-line ()
    (interactive)
    (join-line -1))
  (defun pop-to-mark-command--around (orig-fun &rest args)
    (let ((p (point)))
      (dotimes (i 10)
        (when (= p (point)) (apply orig-fun args)))))
  (advice-add 'pop-to-mark-command :around
              #'pop-to-mark-command--around)
  :config
  (defhydra ptrv-errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first"))
  (setq set-mark-command-repeat-pop t
        column-number-mode t
        transient-mark-mode t
        shift-select-mode t
        next-error-highlight t
        next-error-highlight-no-select t
        save-interprogram-paste-before-kill t))

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      echo-keystrokes 0.02
      inhibit-startup-message t
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash t
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      font-lock-maximum-decoration t
      ;; color-theme-is-global t
      ring-bell-function 'ignore
      x-select-enable-clipboard t
      ;; from https://github.com/technomancy/better-defaults
      x-select-enable-primary nil
      mouse-yank-at-point t)

(use-package newcomment
  :bind (("C-c ; d" . comment-dwim)
         ("C-c ; l" . comment-line)
         ("C-c ; r" . comment-region)))

(use-package minibuffer
  :config
  (setq completion-cycle-threshold 5))

(use-package apropos
  :bind (("C-c h a" . apropos)
         ("C-c h A" . apropos-command)))

(use-package autoinsert
  :bind (("C-c i a" . auto-insert)))

(use-package copyright
  :bind (("C-c i c" . copyright-update))
  :init
  ;; Update copyright when visiting files
  (defun ptrv/copyright-update ()
    (interactive)
    (unless buffer-read-only
      (copyright-update nil 'interactive)
      (unless copyright-update
        ;; Fix years when the copyright information was updated
        (copyright-fix-years))))
  ;; (add-hook 'find-file-hook #'ptrv/copyright-update)
  :config
  ;; Use ranges to denote consecutive years
  (setq copyright-year-ranges t
        ;; Limit copyright changes to my own copyright
        copyright-names-regexp (regexp-quote user-full-name)))

(use-package jka-cmpr-hook
  :config
  (auto-compression-mode)
  (when *is-mac*
    ;; Allow editing of binary .plist files.
    (add-to-list 'jka-compr-compression-info-list
                 ["\\.plist$"
                  "converting text XML to binary plist"
                  "plutil"
                  ("-convert" "binary1" "-o" "-" "-")
                  "converting binary plist to text XML"
                  "plutil"
                  ("-convert" "xml1" "-o" "-" "-")
                  nil nil "bplist"])
    ;;It is necessary to perform an update!
    (jka-compr-update)))

(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :init (winner-mode))

(use-package windmove
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              "/itsalltext/"  ; It's all text temp files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'"
                              "ido.last")))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :demand t
  :config
  (if (fboundp 'save-place-mode)
      (save-place-mode 1)
    (setq-default save-place t)))

(setq history-length 1000)
(use-package savehist
  :demand t
  :config
  (savehist-mode t)
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-save-minibuffer-history t))

;; desktop.el
(use-package desktop
  :demand t
  :config
  (desktop-save-mode)
  (setq desktop-save 'if-exists))

(use-package whitespace
  :bind ("C-c t w" . whitespace-mode)
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing)
        whitespace-line-column nil      ; Use `fill-column' for overlong lines
        whitespace-global-modes '(not go-mode))
  :diminish whitespace-mode)

(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t W" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-cleanup-mode))
  :config
  (setq whitespace-cleanup-mode-only-if-initially-clean t)
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'go-mode))

;; disabled commands
(setq disabled-command-function nil)

;;enable cua-mode for rectangular selections
(use-package cua-base
  :config (setq cua-enable-cua-keys nil))

(defun ptrv/get-default-sound-command ()
  "Get default command for playing sound files."
  (cond
   (*is-mac* (executable-find "afplay"))
   (*is-linux* (executable-find "paplay"))))

(use-package "mule-cmds"
  :config
  (bind-key "C-x <return>" mule-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * compilation
;; Compilation from Emacs
(use-package compile
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :init
  (defun ptrv/show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))
  (bind-key "C-. c" #'ptrv/show-compilation)
  :config
  (require 'ansi-color)
  (defun ptrv/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-process-output nil)
      (setq-local comint-last-output-start (point-marker))))
  (add-hook 'compilation-filter-hook
            #'ptrv/colorize-compilation-buffer)
  ;; other settings
  (setq compilation-scroll-output t
        compilation-always-kill t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * email
(setq user-full-name "Peter Vasil"
      user-mail-address "mail@petervasil.net")

(use-package message
  :config
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :config
  (setq smtpmail-smtp-user user-mail-address
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "mail.petervasil.net"
        smtpmail-smtp-server "mail.petervasil.net"
        smtpmail-smtp-service 587))

(use-package tbemail
  :load-path "site-lisp"
  :mode ("\.eml$" . tbemail-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * timdate
(use-package time
  :bind (("C-c a i" . emacs-init-time)
         ("C-c a t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("America/Los_Angeles" "San Francisco")
                                  ("Europe/Bucharest"  "Satu Mare")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))

(use-package calendar
  :bind ("C-c a c" . calendar)
  :config
  (defconst holiday-german--national-holidays
    '((holiday-fixed 1 1 "Neujahr")
      (holiday-easter-etc -2 "Karfreitag")
      (holiday-easter-etc 1 "Ostermontag")
      (holiday-fixed 5 1 "Tag der Arbeit")
      (holiday-easter-etc 39 "Christi Himmelfahrt")
      (holiday-easter-etc 50 "Pfingstmontag")
      (holiday-fixed 10 3 "Tag der Deutschen Einheit")
      (holiday-fixed 12 25 "1. Weihnachtstag")
      (holiday-fixed 12 26 "2. Weihnachtstag")))
  (setq calendar-week-start-day 1
        calendar-date-style 'european
        calendar-holidays holiday-german--national-holidays
        calendar-mark-holidays-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * calc
(use-package calc
  :bind ("C-c a r" . calc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * diff
(use-package diff
  :bind (("C-c D d" . diff)
         ("C-c D f" . diff-buffer-with-file))
  :config
  (setq diff-switches "-u"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ediff
(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * spelling
(use-package ispell
  :config
  (setq ispell-program-name (if *is-mac*
                                (executable-find "aspell")
                              (executable-find "hunspell"))
        ;; ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "en_US"       ; default dictionary
        ispell-silently-savep t)       ; Don't ask when saving the private dict
  (unless ispell-program-name
    (warn "No spell checker available!")))

(use-package flyspell
  :bind (("C-c t s" . flyspell-mode)
         ("C-c l f" . ptrv/flyspell/body))
  :init
  (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook 'turn-on-flyspell))
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (defhydra ptrv/flyspell (:hint nil)
    "
  _,_:  next error  _._: correct word   _w_: ispell word
-^-----^---------------------^-----^---------------------
  _c_:  correct before point  _p_:  correct previous word
  _b_:  flyspell buffer       _q_:  quit
"
    ("q" nil)
    ("," flyspell-goto-next-error)
    ("." flyspell-auto-correct-word)
    ("w" ispell-word)
    ("c" flyspell-correct-word-before-point)
    ("p" flyspell-auto-correct-previous-word)
    ("b" flyspell-buffer))
  (setq flyspell-use-meta-tab nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (dolist (key '("C-." "C-,"))
    (define-key flyspell-mode-map (kbd key) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * hippie-expand
;; http://trey-jackson.blogspot.de/2007/12/emacs-tip-5-hippie-expand.html
(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
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
          try-complete-lisp-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * look-and-feel
(use-package rainbow-mode
  :ensure t
  :bind ("C-c t r" . rainbow-mode)
  :diminish rainbow-mode)

(setq column-number-mode t)
(use-package hl-line
  :init (global-hl-line-mode))

(use-package zeburn
  :ensure zenburn-theme
  :init (load-theme 'zenburn :no-confirm))


(defvar headline-face 'headline-face)
(defface headline-face
  '((t (:inherit font-lock-comment-face
                 :weight bold
                 :bold t
                 :underline t)))
  "Face for headlines."
  :group 'org-faces)

(defun fontify-headline ()
  "Fontify certain headlines."
  (font-lock-add-keywords
   nil '(("^;;;; [* ]*\\(.*\\)\\>"
          (1 headline-face t)))))

(use-package hl-todo
  :ensure t
  :bind ("C-c l t" . ptrv/todos/body)
  :init (global-hl-todo-mode)
  :config
  (defhydra ptrv/todos ()
    "Todos"
    ("n" hl-todo-next "next")
    ("p" hl-todo-previous "previous")))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * beacon
(use-package beacon
  :ensure t
  :init (add-hook 'after-init-hook #'beacon-mode)
  :diminish beacon-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * helm
(use-package helm
  :ensure t
  :bind (("C-. ." . helm-resume))
  :init
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config (setq helm-split-window-in-side-p t)
  :diminish helm-mode)

(use-package helm-misc
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-command
  :ensure helm
  :bind ([remap execute-extended-command] . helm-M-x)
  :config (setq helm-M-x-fuzzy-match t))

(use-package helm-buffers
  :ensure helm
  :config (setq helm-buffers-fuzzy-matching t))

(use-package helm-files
  :ensure helm
  :bind ([remap find-file] . helm-find-files)
  :config
  (setq ;;helm-recentf-fuzzy-match t
   ;; Use recentf to find recent files
   helm-ff-file-name-history-use-recentf t
   ;; Find library from `require', `declare-function' and friends
   helm-ff-search-library-in-sexp t
   helm-ff-fuzzy-matching t))

(use-package helm-for-files
  :ensure helm
  :bind (("C-c f s" . helm-for-files)
         ("C-c f r" . helm-recentf))
  :config
  (when *is-mac*
    ;; Replace locate with spotlight for `helm-for-files'
    (setq helm-for-files-preferred-list
          (append (delq 'helm-source-locate
                        helm-for-files-preferred-list)
                  '(helm-source-mac-spotlight)))))

(use-package helm-imenu
  :ensure helm
  :bind (;; ("C-c n i" . helm-imenu-in-all-buffers)
         ;; ("C-c n t" . helm-imenu)
         ("C-x C-i" . helm-imenu))
  :config (setq helm-imenu-fuzzy-match t
                ;; Don't automatically jump to candidate if only one match,
                ;; because it makes the behaviour of this command unpredictable,
                ;; and prevents me from getting an overview over the buffer if
                ;; point is on a matching symbol.
                helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-ring
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package helm-elisp
  :ensure helm
  :bind (;; ("C-c h l" . helm-locate-library)
         ("C-c h a" . helm-apropos)))

(use-package helm-color
  :ensure helm
  :bind ("C-c i C" . helm-colors))

(use-package helm-ag
  :ensure t
  ;; :bind (("C-c s a" . helm-do-ag)
  ;;        ("C-c s A" . helm-ag))
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :ensure t
  :bind ("C-c s p" . helm-projectile-ag)
  :init (helm-projectile-on)
  :config
  (setq projectile-switch-project-action #'helm-projectile)

  (bind-key "C-t" #'ptrv/neotree-project-root
            helm-projectile-projects-map)

  (helm-add-action-to-source "Open NeoTree `C-t'"
                             #'ptrv/neotree-project-root
                             helm-source-projectile-projects 1))

(use-package helm-info
  :ensure helm
  :bind (("C-c h e" . helm-info-emacs)
         ("C-c h i" . helm-info-at-point)))

(use-package helm-man
  :ensure helm
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-ls-git
  :ensure t
  :bind ("C-c g h" . helm-ls-git-ls))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s s" . helm-swoop)
         ("C-c s S" . helm-multi-swoop)
         ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t))

(use-package helm-gitignore
  :ensure t
  :bind ("C-c g I" . helm-gitignore))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :init
  (with-eval-after-load 'projectile
    (defun ptrv/neotree-project-root (&optional directory)
      "Open a NeoTree browser for a project DIRECTORY."
      (interactive)
      (let ((default-directory (or directory default-directory)))
        (if (and (fboundp 'neo-global--window-exists-p)
                 (neo-global--window-exists-p))
            (neotree-hide)
          (neotree-find (projectile-project-root)))))
    (bind-key "t" #'ptrv/neotree-project-root
              projectile-command-map))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t))

(use-package smex
  :ensure t)

(use-package helm-smex
  :load-path "site-lisp/helm-smex"
  :bind (([remap execute-extended-command] . helm-smex)
         ("M-X" . helm-smex-major-mode-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ido
(use-package ido
  :disabled t
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  :bind (("C-x M-f" . ido-find-file-other-window))
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-max-directory-size 100000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ido-ubiquitous
(use-package ido-ubiquitous
  :disabled t
  :ensure t
  :config
  (ido-ubiquitous-mode)
  (dolist (cmd '(sh-set-shell
                 ispell-change-dictionary
                 add-dir-local-variable
                 ahg-do-command
                 sclang-dump-interface
                 sclang-dump-full-interface
                 kill-ring-search
                 tmm-menubar
                 erc-iswitchb
                 iswitchb-buffer))
    (add-to-list 'ido-ubiquitous-command-overrides
                 `(disable exact ,(symbol-name cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * smex
(use-package smex
  :disabled t
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * idomenu
(use-package idomenu
  :disabled t
  :ensure t
  :bind (("C-x C-i" . idomenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pages
(use-package page
  :bind (("C-x ]" . ptrv-pages/forward-page)
         ("C-x [" . ptrv-pages/backward-page))
  :config
  (defhydra ptrv-pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Eshell
(use-package eshell
  :bind (("C-x m" . eshell))
  :init
  ;; http://irreal.org/blog/?p=1742
  (defun ptrv/eshell-or-restore ()
    "Bring up a full-screen eshell or restore previous config."
    (interactive)
    (if (string= "eshell-mode" major-mode)
        (jump-to-register :eshell-fullscreen)
      (window-configuration-to-register :eshell-fullscreen)
      (eshell)
      (delete-other-windows)))
  :config
  (setq eshell-directory-name (locate-user-emacs-file "eshell/"))

  (bind-key "C-x M" (lambda () (interactive) (eshell t)))

  (defun eshell/clear ()
    "04Dec2001 - sailor, to clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun eshell/e (file)
    (find-file file))

  (use-package pcmpl-lein
    :load-path "site-lisp")

  (use-package pcmpl-git
    :ensure t))

(use-package shell
  :bind ("C-c a S" . shell))

(use-package term
  :bind ("C-c a s" . ansi-term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * company
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-show-numbers t
        company-global-modes '(not magit-status-mode)
        ;; company-transformers '(company-sort-by-occurrence))
        )
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (bind-key [remap complete-symbol] #'company-complete company-mode-map))

;; (use-package company-statistics
;;   :ensure t
;;   :init (company-statistics-mode))

(use-package company-dabbrev
  :ensure company
  :config (setq company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :after company
  :init (company-quickhelp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * paredit
(use-package paredit
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  eval-expression-minibuffer-setup-hook))
    (add-hook hook #'enable-paredit-mode))
  :config
  (with-eval-after-load 'eldoc
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)))

(use-package elec-pair
  :demand t
  :config (electric-pair-mode +1))

(use-package paren
  :demand t
  :config
  (setq show-paren-style 'parenthesis)
  (show-paren-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * smartparens
(use-package smartparens
  :disabled t
  :ensure t
  :demand t
  :bind (:map smartparens-strict-mode-map
              ("M-q" . sp-indent-defun)
              ("C-j" . sp-newline)
              ("M-?" . sp-convolute-sexp))
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (dolist (hook '(ielm-mode-hook
                  emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook))
    (add-hook hook #'smartparens-strict-mode))
  (require 'smartparens-config)

  (dolist (it sp-paredit-bindings)
    (bind-key (car it) (cdr it) smartparens-strict-mode-map))

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair sp-lisp-modes "(" nil :bind "M-(")

  (sp-local-pair '(c++-mode go-mode) "{" nil :post-handlers
                 '(((lambda (&rest _ignored)
                      (ptrv/smart-open-line-above)) "RET"))))

(use-package embrace
  :ensure t
  :bind (("C-c x e" . ptrv/embrace/body))
  :config
  (defhydra ptrv/embrace (:hint nil)
    "
Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
"
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * elisp
(use-package lisp-mode
  :mode (("\\.el$" . emacs-lisp-mode))
  :config
  (with-eval-after-load 'company
    (defun ptrv/company-elisp--init ()
      (setq-local company-backends '((company-capf :with company-dabbrev))))
    (add-hook 'emacs-lisp-mode-hook 'ptrv/company-elisp--init))

  (defun ptrv/remove-elc-on-save ()
    "If you’re saving an elisp file, likely the .elc is no longer valid."
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))
              nil :local))

  (dolist (it '(ptrv/remove-elc-on-save fontify-headline))
    (add-hook 'emacs-lisp-mode-hook it))

  (use-package ielm
    :init
    (defun ptrv/switch-to-ielm ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    (bind-key "C-c a '" 'ptrv/switch-to-ielm))

  (bind-key "C-c C-p" 'eval-print-last-sexp lisp-mode-shared-map)
  (bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)

  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
  (bind-key "C-c C-e" 'eval-and-replace lisp-mode-shared-map)

  (defun ptrv/lisp-describe-thing-at-point ()
    "Show the documentation of the Elisp function and variable near point.

This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call"
    (interactive)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (let ((sym (ignore-errors
                 (with-syntax-table emacs-lisp-mode-syntax-table
                   (save-excursion
                     (or (not (zerop (skip-syntax-backward "_w")))
                         (eq (char-syntax (char-after (point))) ?w)
                         (eq (char-syntax (char-after (point))) ?_)
                         (forward-sexp -1))
                     (skip-chars-forward "`'")
                     (let ((obj (read (current-buffer))))
                       (and (symbolp obj) (fboundp obj) obj)))))))
      (if sym (describe-function sym)
        (describe-variable (variable-at-point)))))
  (bind-key "M-RET" 'ptrv/lisp-describe-thing-at-point lisp-mode-shared-map)

  (use-package lexbind-mode
    :ensure t
    :init (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

  (with-eval-after-load 'ptrv-simple
    (bind-key "C-M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map)))

(use-package elisp-slime-nav
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
     (add-hook hook #'elisp-slime-nav-mode))
  :diminish elisp-slime-nav-mode)

(use-package eldoc
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  eval-expression-minibuffer-setup-hook
                  c-mode-common-hook
                  python-mode-hook
                  cider-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook #'eldoc-mode))
  :diminish eldoc-mode)

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package macrostep
  :ensure t
  :init
  (bind-key "C-c m e" #'macrostep-expand emacs-lisp-mode-map)
  (bind-key "C-c m e" #'macrostep-expand lisp-interaction-mode-map))

(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package eval-sexp-fu
  :ensure t
  :demand t
  :init
  (when (>= emacs-major-version 25)
    (eval-after-load 'bytecomp
      '(add-to-list 'byte-compile-not-obsolete-funcs
                    'preceding-sexp)))
  :config
  (turn-on-eval-sexp-fu-flash-mode))

(use-package cask-mode
  :ensure t)

(use-package bug-hunter
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * clojure
(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t)

  (with-eval-after-load 'ptrv-simple
    (bind-key "C-M-;" #'comment-or-uncomment-sexp clojure-mode-map))

  (defun ptrv/cider-doc ()
    (interactive)
    (cider-doc t))

  (bind-key "M-RET" #'ptrv/cider-doc cider-mode-map)

  (unbind-key "C-<return>" cider-repl-mode-map)
  (bind-keys :map cider-repl-mode-map
             ("M-RET" . ptrv/cider-doc)
             ("C-M-<return>" . cider-repl-closing-return))

  (setq cider-repl-use-clojure-font-lock t)

  (add-hook 'cider-repl-mode-hook #'subword-mode)

  (use-package cider-eval-sexp-fu
    :ensure t
    :demand t)

  (use-package clj-refactor
    :disabled t
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * tramp
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

(use-package tramp
  :config
  (setq tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ffap
(use-package ffap
  :bind ("C-c j f" . ffap)
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ibuffer
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :init (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  :config
  (use-package ibuf-ext
    :config (setq ibuffer-show-empty-filter-groups nil))
  (defun ptrv/ibuffer-quit (&optional bury window)
    (interactive "P")
    (quit-window (not bury) window))
  (bind-key "q" #'ptrv/ibuffer-quit ibuffer-mode-map))

(use-package ibuffer-projectile
  :ensure t
  :init
  (defun ptrv/ibuffer-group-buffers ()
    (setq ibuffer-filter-groups
          (append
           '(("IRC" (mode . erc-mode))
             ("Help" (or (name . "\\*Help\\*")
                         (name . "\\*Apropos\\*")
                         (name . "\\*info\\*")))
             ("Emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*Backtrace\\*$")
                          (mode . inferior-emacs-lisp-mode)))
             ("Helm" (name . "^\\*helm.*"))
             ("root" (filename . "^/sudo:root.*"))
             ("Org" (mode . org-mode)))
           (ibuffer-projectile-generate-filter-groups)))
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook
            #'ptrv/ibuffer-group-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * gist
(use-package yagist
  :ensure t
  :bind(("C-c g g c" . yagist-region-or-buffer)
        ("C-c g g p" . yagist-region-or-buffer-private)
        ("C-c g g l" . yagist-list))
  :config (setq yagist-view-gist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g M-g" . magit-dispatch-popup)
         ("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g F" . magit-pull))
  :init
  (setq magit-push-current-set-remote-if-missing nil)
  :config
  ;; hide stashes section in magit status
  (add-hook 'magit-section-set-visibility-hook
            (lambda (section)
              (and (memq (magit-section-type section) '(stashes))
                   'hide)))

  (defun ptrv/magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because Magit adds
      ;; trailing slashes again, which breaks the presentation in the Magit
      ;; prompt.
      (require 'cl)
      (setq magit-repository-directories
            (remove-if (lambda (dir)
                         (file-remote-p dir 'method))
                       (mapcar #'directory-file-name project-dirs)))))

  (with-eval-after-load 'projectile
    (ptrv/magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'ptrv/magit-set-repo-dirs-from-projectile))

(use-package git-commit
  :ensure t
  :commands (git-commit-setup-check-buffer)
  :init (add-hook 'find-file-hook 'git-commit-setup-check-buffer)
  :config
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package gitattributes-mode
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind ("C-c g T" . git-timemachine-toggle))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c m m" . hydra-smerge/body))
  :config
  (defhydra hydra-smerge
    (:color red :hint nil)
    "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
    ("RET" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("E" smerge-ediff)
    ("R" smerge-refine)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("n" smerge-next)
    ("o" smerge-keep-other)
    ("p" smerge-prev)
    ("r" smerge-resolve)
    ("<" smerge-diff-base-mine)
    ("=" smerge-diff-mine-other)
    (">" smerge-diff-base-other)
    ("q" nil :color blue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * vc
(use-package vc-hooks
  :config (setq vc-follow-symlinks t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * git-messenger
(use-package git-messenger
  :ensure t
  :bind (("C-c g p" . git-messenger:popup-message))
  :config (setq git-messenger:show-detail t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * diff-hl
(use-package diff-hl
  :ensure t
  :bind (:map diff-hl-command-map
              ("]" . ptrv-hunks/diff-hl-next-hunk)
              ("[" . ptrv-hunks/diff-hl-previous-hunk))
  :init
  (global-diff-hl-mode)
  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (defhydra ptrv-hunks ()
    "Hunks"
    ("]" diff-hl-next-hunk "next")
    ("[" diff-hl-previous-hunk "previous")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * mercurial
(use-package ahg
  :ensure t
  :commands (ahg-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * yasnippet
;; (yas-global-mode 1)
(use-package yasnippet
  :ensure t
  :mode ("\\.yasnippet$" . yasnippet-mode)
  :init
  (dolist (hook '(lua-mode-hook
                  c++-mode-hook
                  sclang-mode-hook
                  processing-mode-hook
                  go-mode-hook
                  clojure-mode-hook
                  org-mode-hook))
    (add-hook hook #'yas-minor-mode))
  :config
  (setq yas-prompt-functions '(yas-x-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  (unless yas-global-mode (yas-reload-all))

  (use-package dropdown-list
    :ensure t
    :init
    (add-to-list 'yas-prompt-functions 'yas-dropdown-prompt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * undo-tree
(use-package undo-tree
  :demand t
  :ensure t
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pomodoro.el
(use-package pomodoro
  :ensure t
  :config
  (pomodoro-add-to-mode-line)
  (setq pomodoro-sound-player (ptrv/get-default-sound-command)
        pomodoro-break-start-sound (expand-file-name "sounds/alarm.wav" ptrv/etc-dir)
        pomodoro-work-start-sound (expand-file-name "sounds/alarm.wav" ptrv/etc-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * sql-mode
(use-package sql
  :config
  (bind-keys :map sql-mode-map
             ("C-c m p" . sql-set-product)
             ("C-c m i" . sql-set-sqli-buffer)))

(use-package sql-spatialite-ext
  :commands (sql-spatialite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * frames
;; display visited file's path as frame title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;; defalias
(defalias 'toggle-fullscreen 'toggle-frame-maximized)

(use-package frame
  :bind (("C-c w F" . toggle-frame-fullscreen)
         ("C-c w M" . toggle-frame-maximized)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * fullframe
(use-package fullframe
  :ensure t
  :init (fullframe magit-status magit-mode-quit-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * iflipb
(use-package iflipb
  :ensure t
  :bind (("C-<next>"      . iflipb-next-buffer)
         ("C-<prior>"     . iflipb-previous-buffer)
         ("<XF86Forward>" . iflipb-next-buffer)
         ("<XF86Back>"    . iflipb-previous-buffer))
  :config
  (setq iflipb-ignore-buffers
        '("*Help*"
          "*Compile-Log*"
          "*Ibuffer*"
          "*Messages*"
          "*Completions*"
          "*magit"
          "*Pymacs*"
          "*clang-complete*"
          "*compilation*"
          "*Packages*"
          "*file-index*"
          " output*"
          "*tramp/"
          "*project-status*"
          "SCLang:PostBuffer*")
        iflipb-wrap-around t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * search
(use-package "isearch"
  :bind ([remap isearch-forward] . ptrv/isearch-symbol-with-prefix)
  :init
  (defun ptrv/isearch-symbol-with-prefix (p)
    "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
    (interactive "P")
    (let ((current-prefix-arg nil))
      (call-interactively
       (if p #'isearch-forward-symbol-at-point #'isearch-forward))))
  :config
  (bind-key "C-o" (lambda () (interactive)
                    (let ((case-fold-search isearch-case-fold-search))
                      (occur (if isearch-regexp
                                 isearch-string
                               (regexp-quote isearch-string)))))
            isearch-mode-map))

(use-package highlight-symbol
  :ensure t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ;; ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ;; ("C-c s p" . highlight-symbol-prev-in-defun)
   )
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

;; the silver searcher
(use-package ag
  :ensure t
  :init
  :bind(("C-c s A" . ag-regexp)
        ("C-c s a" . ag))
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package wgrep-ag
  :ensure t)

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :diminish anzu-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * which-function
(use-package which-func
  :init (which-function-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * edit-server
(use-package edit-server
  :ensure t
  :if window-system
  :init
  (edit-server-start)
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
  :config
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))
        edit-server-new-frame nil))

(use-package edit-server-htmlize
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * iedit
(use-package iedit
  :disabled t
  :ensure t
  :bind ("C-c s i" . iedit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * google-this
(use-package google-this
  :ensure t
  :init (google-this-mode)
  :diminish google-this-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * popwin
(use-package popwin
  :ensure t
  :demand t
  :config
  (popwin-mode)
  (bind-key "C-z" popwin:keymap)

  (defun ptrv/get-popwin-height (&optional size)
    (let* ((default-values (cond ((>= (display-pixel-height) 1000) '(30 20 15))
                                 ((and (< (display-pixel-height) 1000)
                                       (>= (display-pixel-height) 900)) '(25 20 15))
                                 ((< (display-pixel-height) 900) '(20 15 10)))))
      (cond ((eq size 'small) (nth 2 default-values))
            ((eq size 'medium) (nth 1 default-values))
            ((eq size 'big) (car default-values))
            (:else (car default-values)))))

  (setq popwin:special-display-config
        `((help-mode :height ,(ptrv/get-popwin-height) :stick t)
          ("*Completions*" :noselect t)
          ("*compilation*" :noselect t :height ,(ptrv/get-popwin-height))
          ("*Messages*")
          ("*Occur*" :noselect t)
          ("\\*Slime Description.*" :noselect t :regexp t :height ,(ptrv/get-popwin-height))
          ("*magit-commit*" :noselect t :height ,(ptrv/get-popwin-height) :width 80 :stick t)
          ("COMMIT_EDITMSG" :noselect t :height ,(ptrv/get-popwin-height) :width 80 :stick t)
          ("*magit-diff*" :noselect t :height ,(ptrv/get-popwin-height) :width 80)
          ("*magit-edit-log*" :noselect t :height ,(ptrv/get-popwin-height 'small) :width 80)
          ("*magit-process*" :noselect t :height ,(ptrv/get-popwin-height 'small) :width 80)
          ("\\*Slime Inspector.*" :regexp t :height ,(ptrv/get-popwin-height))
          ("*Ido Completions*" :noselect t :height ,(ptrv/get-popwin-height))
          ;;("*eshell*" :height 20)
          ("\\*ansi-term\\*.*" :regexp t :height ,(ptrv/get-popwin-height))
          ("*shell*" :height ,(ptrv/get-popwin-height))
          (".*overtone.log" :regexp t :height ,(ptrv/get-popwin-height))
          ("*gists*" :height ,(ptrv/get-popwin-height))
          ("*sldb.*":regexp t :height ,(ptrv/get-popwin-height))
          ("*Gofmt Errors*" :noselect t)
          ("\\*godoc" :regexp t :height ,(ptrv/get-popwin-height))
          ("*Shell Command Output*" :noselect t)
          ("*cider-doc*" :height ,(ptrv/get-popwin-height 'medium) :stick t)
          ("\\*cider-repl " :regexp t :height ,(ptrv/get-popwin-height 'medium) :stick t)
          ("*Kill Ring*" :height ,(ptrv/get-popwin-height))
          ("*project-status*" :noselect t)
          ("*pytest*" :noselect t)
          ("*Python*" :stick t)
          ("*Python Doc*" :noselect t)
          ("*jedi:doc*" :noselect t)
          ("*Registers*" :noselect t)
          ("*ielm*" :stick t)
          ("*Flycheck errors*" :stick t :noselect t)
          ("*processing-compilation*" :noselect t)
          ("*anaconda-doc*" :noselect t)
          ("*company-documentation*" :noselect t :height ,(ptrv/get-popwin-height 'small))
          ("*wclock*" :noselect t :height ,(ptrv/get-popwin-height 'small))
          ("*cscope*" :height ,(ptrv/get-popwin-height 'medium))
          ("*xref*" :height ,(ptrv/get-popwin-height 'medium)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * buffer
(use-package autorevert
  :config
  (global-auto-revert-mode)
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(setq tab-stop-list '(2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64
                        68 72 76 80 84 88 92 96 100 104 108 112 116 120))
;; (customize-set-variable
;;  'tab-stop-list '(2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64
;;                     68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; make file executabable on save if has shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c o a" . org-agenda)
         ("C-c o b" . org-iswitchb)
         ("C-c o l" . org-store-link))
  :config
  (defvar ptrv/org-agenda-file "~/org/agenda.org")
  (setq org-outline-path-complete-in-steps nil
        org-log-done t
        org-src-fontify-natively nil
        org-default-notes-file "~/org/inbox.org"
        org-agenda-files `(,org-default-notes-file
                           "~/org/gtd.org"
                           "~/org/tickler.org"
                           ,ptrv/org-agenda-file)
        org-refile-use-outline-path 'file
        org-refile-targets `(("~/org/gtd.org" :maxlevel . 3)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/tickler.org" :maxlevel . 2)
                             ("~/org/inbox.org" :maxlevel . 1)
                             (,ptrv/org-agenda-file :level . 1))
        org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "FEATURE(f)" "|" "COMPLETED(c)")
          (sequence "BUG(b)" "|" "FIXED(x)")
          (sequence "APPT(p)" "|" "DONE(d)" "CANCELLED(a)")
          ;; (sequence "WAITING(w!)" "|" "DONE(d)")
          ))

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (with-eval-after-load 'yasnippet
    (defun yas-org-very-safe-expand ()
      (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
    (defun org-mode-yasnippet-workaround ()
      (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand))
    (add-hook 'org-mode-hook 'org-mode-yasnippet-workaround))

  ;; (defun org-mode-init ()
  ;;   (turn-off-flyspell))
  ;; (add-hook 'org-mode-hook 'org-mode-init)
  )

(use-package org-agenda
  :ensure org
  :config
  (defun org-agenda-format-parent (n)
    ;; (s-truncate n (org-format-outline-path (org-get-outline-path)))
    (save-excursion
      (save-restriction
        (widen)
        (org-up-heading-safe)
        (s-truncate n (org-get-heading t t)))))
  ;; Custom agenda function, see customized 'org-agenda-custom-commands.
  ;; Open the agenda in org-agenda-log-mode, with the archive file, and
  ;; for the previous week. To work well, 'org-log-done should be set to
  ;; 't, to ensure that everything is in the agenda.
  (defun ptrv/org-agenda-log (arg)
    (org-agenda-archives-mode)
    (org-agenda-list arg)
    (org-agenda-log-mode)
    (org-agenda-earlier 1))

  (setq org-agenda-custom-commands
        '(("d" "30 days deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-overriding-header "Month deadlines")
            (org-agenda-span 'month)
            (org-agenda-overriding-header "")))
          ("l" "Logbook" ptrv/org-agenda-log ""
           ((org-agenda-overriding-header "Logbook")))
          ("E" "Errands" tags "errands" nil)
          ("n" "Next actions"
           ((alltodo ""
                     ((org-agenda-tag-filter-preset nil)
                      (org-agenda-overriding-header "Next actions")
                      (org-agenda-skip-function
                       #'my-org-agenda-skip-all-siblings-but-first)
                      (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")
                      (org-agenda-todo-keyword-format "%-4s")
                      (org-agenda-files '("~/org/gtd.org")))))
           nil nil)
          ("@" "Contexts"
           ((todo "WAITING" ((org-agenda-overriding-header "Waiting")))
            (tags-todo "@email"
                       ((org-agenda-overriding-header "Emails")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'nottodo '("TODO")))))
            (tags-todo "@phone"
                       ((org-agenda-overriding-header "Phone calls")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "@office"
                       ((org-agenda-overriding-header "At the office")
                        (org-agenda-skip-function
                         #'my-org-agenda-skip-all-siblings-but-first)))
            (tags-todo "@freelance"
                       ((org-agenda-overriding-header "Freelance")
                        (org-agenda-skip-function
                         #'my-org-agenda-skip-all-siblings-but-first)))
            (tags-todo "@home"
                       ((org-agenda-overriding-header "Home")
                        (org-agenda-skip-function
                         #'my-org-agenda-skip-all-siblings-but-first)))
            nil nil))))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state))))

(use-package org-clock
  :ensure org
  :config (setq org-clock-into-drawer t))

(use-package org-mobile
  :ensure org
  :config
  (setq org-mobile-directory "~/Dropbox/MobileOrg"
        org-mobile-files '("~/org/inbox.org"
                           "~/org/gtd.org"
                           "~/org/someday.org"
                           "~/org/tickler.org")
        org-mobile-inbox-for-pull "~/org/from-mobile.org"))

(use-package org-capture
  :ensure org
  :bind ("C-c o c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %i%?")
          ("l" "Todo (with link) [inbox]" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %a")
          ("T" "Tickler" entry
           (file+headline "~/org/tickler.org" "Tickler")
           "* %i%? \n %U")
          ("p" "Appointment" entry
           (file+headline ptrv/org-agenda-file "Appointment")
           "* APPT %i%? \n %^t"))))

(use-package ox
  :ensure org
  :config (load "~/.org-publish-projects.el" 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org2blog
(use-package org2blog
  :ensure t
  :config (load "~/.org-blogs.el" 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org-present
(use-package org-present
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org-reveal
(use-package ox-reveal
  :ensure t
  :after org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * info-look
(use-package info-look
  :commands info-lookup-add-help)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * latex
(use-package tex-site
  :disabled t
  :ensure auctex)

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t)

  (setq-default TeX-master nil
                TeX-PDF-mode t
                TeX-command-default "latexmk")

  (dolist (cmd '(("latexmk" "latexmk %s" TeX-run-TeX nil
                  (latex-mode doctex-mode) :help "Run latexmk")
                 ("latexmk clean" "latexmk -c %s" TeX-run-TeX nil
                  (latex-mode doctex-mode) :help "Run latexmk -c")
                 ("latexmk cleanall" "latexmk -C %s" TeX-run-TeX nil
                  (latex-mode doctex-mode) :help "Run latexmk -C")))
    (add-to-list 'TeX-command-list cmd t))

  ;; Replace the rotten Lacheck with Chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v5 %s")

  (cond (*is-linux*
         (add-to-list 'TeX-expand-list '("%C" (lambda () (buffer-file-name))) t)
         (setq TeX-view-program-list '(("Okular" "okular --unique %o#src:%n%C")))
         (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular"))))
        (*is-mac*
         (setq TeX-view-program-selection '((output-pdf "Skim")))
         (setq TeX-view-program-list
               '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))))

(use-package latex
  :ensure auctex
  :config
  (dolist (it '(LaTeX-math-mode reftex-mode auto-fill-mode))
    (add-hook 'LaTeX-mode-hook it))

  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))

  ;; clean intermediate files from latexmk
  (dolist (suffix '("\\.fdb_latexmk" "\\.fls"))
    (add-to-list 'LaTeX-clean-intermediate-suffixes suffix))

  (info-lookup-add-help
   :mode 'latex-mode
   :regexp ".*"
   :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
   :doc-spec '(("(latex2e)Concept Index" )
               ("(latex2e)Command Index"))))

(use-package reftex
  :ensure auctex
  :config
  (setq reftex-plug-into-AUCTeX t
        ;; Recommended optimizations
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-ref-style-default-list '("Hyperref")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * filetypes
(use-package sh-script
  :mode (("\\.zsh-template$" . shell-script-mode)
         ("\\.zsh$" . shell-script-mode)
         ("zsh\\.*" . shell-script-mode)))

(use-package css-mode
  :mode ("\\.css$" . css-mode))

(use-package yaml-mode
  :ensure t)

;; pd-mode
(use-package pd-mode
  :mode (("\\.pat$" . pd-mode)
         ("\\.pd$" . pd-mode)))

;; cmake
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; glsl-mode
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

;; IanniX
(use-package js-mode
  :mode ("\\.nxscript$" . js-mode))

;; ChucK
(use-package chuck-mode
  :load-path "site-lisp/chuck-mode"
  :mode ("\\.ck$" . chuck-mode))

;; arduino
(use-package arduino-mode
  :load-path "site-lisp/arduino-mode"
  :mode ("\\.ino\\'" . arduino-mode))

;; arch linux
(use-package pkgbuild-mode
  :ensure t
  :mode ("/PKGBUILD$" . pkgbuild-mode))
;; (ptrv/add-auto-mode 'shell-script-mode "\\.install$")

(use-package conf-mode
  :mode ("\\.*rc$" . conf-unix-mode))

;; json
(use-package json-reformat
  :ensure t
  :bind ("C-c x j" . json-reformat-region))

;; gnuplot
(use-package gnuplot
  :commands (gnuplot-mode gnuplot-make-buffer)
  :mode ("\\.gp$" . gnuplot-mode))

;;;; * abbrev
(use-package abbrev
  :init (setq-default abbrev-mode t)
  :config
  (define-abbrev-table 'global-abbrev-table
    '(
      ;; typo corrections
      ("teh" "the")
      ))
  (setq save-abbrevs nil)
  :diminish abbrev-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\README\\.md" . gfm-mode))
  :config
  (setq markdown-css-paths
        (list
         (expand-file-name "css/pandoc.css" ptrv/etc-dir)))
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pandoc
(use-package pandoc-mode
  :ensure t
  :mode ("\\.text$" . markdown-mode)
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jira
(use-package jira-markup-mode
  :ensure t
  :mode ("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * golang
(use-package go-mode
  :ensure t
  :config
  (bind-keys :map go-mode-map
             ;; ("M-."       . godef-jump)
             ;; ("C-c C-i"   . go-goto-imports)
             ;; ("C-c C-r"   . go-remove-unused-imports)
             ("C-c m p"   . ptrv/go-create-package)
             ;; ("C-c C-c c" . ptrv/go-run)
             ;; ("C-c C-c r" . ptrv/go-run-buffer)
             ;; ("C-c C-c b" . ptrv/go-build)
             ;; ("C-c C-c t" . ptrv/go-test)
             ("C-c m d" . godoc-at-point)
             )

  (use-package ptrv-go
    :load-path "site-lisp"
    :config
    (ptrv/load-file-from-gopath-or-download
     "github.com/golang/lint" "misc/emacs/golint.el"))

  (setq gofmt-command "goimports")

  (defun ptrv/go-mode-init ()
    (add-hook 'before-save-hook 'gofmt-before-save nil :local)

    ;; Customize compile command to run go build
    (if (not (and (stringp compile-command)
                  (string-match "go" compile-command)))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet && golint")))
  (add-hook 'go-mode-hook 'ptrv/go-mode-init)

  (use-package company-go
    :disabled t
    :ensure t
    :init (add-hook 'go-mode-hook
                    (lambda ()
                      (setq-local company-backends
                                  '((company-go :with company-yasnippet)))))
    :config (setq company-go-show-annotation nil))

  (use-package go-eldoc
    :ensure t
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (with-eval-after-load 'flycheck
    (defvar flycheck-check-syntax-automatically)
    (defun ptrv/go-mode-flycheck--init ()
      (setq-local flycheck-check-syntax-automatically '(save)))
    (add-hook 'go-mode-hook 'ptrv/go-mode-flycheck--init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * xml
(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.gpx$" . nxml-mode)
         ("\\.plist$" . nxml-mode))
  :commands (xml-format)
  :bind ("C-c x x" . xml-format)
  :config
  (defun xml-format ()
    "Format XML file with xmllint."
    (interactive)
    (if (executable-find "xmllint")
        (when (eq major-mode 'nxml-mode)
          (save-excursion
            (shell-command-on-region
             (point-min) (point-max) "xmllint --format -" (buffer-name) t)))
      (user-error "The executable `xmllint' not found!")))
  (defun gpx-setup ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.gpx\\'" buffer-file-name))
      (setq-local nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
      (setq-local nxml-heading-element-name-regexp "name\\|time")))
  (add-hook 'nxml-mode-hook 'gpx-setup)

  (setq nxml-slash-auto-complete-flag t
        nxml-sexp-element-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * erc
(use-package erc
  :init
  (defun erc-connect ()
    (interactive)
    (let ((freenode-user (car (netrc-credentials "freenode"))))
      (if freenode-user
          (erc-tls :server "irc.freenode.net"
                   :port 7000
                   :nick freenode-user)
        (user-error "Freenode user-name not found"))))
  :config
  (setq erc-server "irc.freenode.net"
        erc-port 7000
        erc-nick "ptrv"
        erc-nick-uniquifier "_"
        erc-server-connect-function 'erc-open-tls-stream)

  (use-package alert
    :ensure t)

  (use-package erc-hl-nicks
    :ensure t)

  (use-package ptrv-erc
    :load-path "site-lisp"
    :config
    (bind-key "C-c C-b" #'ptrv/erc-switch-to-buffer erc-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * faust-mode
(use-package faust-mode
  :load-path "site-lisp/emacs-faust-mode"
  :mode ("\\.dsp$" . faust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Synth-A-Modeler mode
(use-package sam-mode
  :load-path "site-lisp"
  :mode ("\\.mdl$" . sam-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * editing
(setq sentence-end-double-space nil)

(use-package subword
  :init (add-hook 'prog-mode-hook 'subword-mode)
  :diminish subword-mode)

(use-package delsel
  :init (delete-selection-mode))

;;fast vertical naviation
(bind-key "M-U" (lambda () (interactive) (forward-line -10)))
(bind-key "M-D" (lambda () (interactive) (forward-line 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * bug-reference
(use-package bug-reference
  :init
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode)
  (add-hook 'text-mode-hook 'bug-reference-mode)
  (with-eval-after-load 'magit
    (dolist (hook '(magit-status-mode-hook magit-log-mode-hook))
      (add-hook hook #'bug-reference-mode))
    (dolist (hook '(magit-status-mode-hook magit-log-mode-hook))
      (add-hook hook #'hack-dir-local-variables-non-file-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * move-text
(use-package move-text
  :ensure t
  :bind (("C-S-<up>"   . move-text-up)
         ("C-S-<down>" . move-text-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * file commands
(use-package ptrv-files
  :load-path "site-lisp"
  :bind (("<f5>"      . ptrv/refresh-file)
         ;; ("C-c f r"   . ptrv/ido-recentf-open)
         ("C-c f o"   . ptrv/open-with)
         ("C-c f d"   . ptrv/launch-directory)
         ("C-c f R"   . ptrv/rename-current-buffer-file)
         ("C-c f D"   . ptrv/delete-file-and-buffer)
         ("C-c f w"   . ptrv/copy-file-name-to-clipboard)
         ("C-c f i"   . ptrv/find-user-init-file)
         ("C-c f I"   . ptrv/find-user-custom-file)
         ("C-c f b i" . ptrv/byte-recompile-init)
         ("C-c f b s" . ptrv/byte-recompile-site-lisp)
         ("C-c f b e" . ptrv/byte-recompile-elpa)
         ("C-c f b h" . ptrv/byte-recompile-home)))

(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * dired
(use-package dired
  :config
  (require 'dired-x)
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF"
        dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v")))
  (defun ptrv/dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    ;; store all selected files into "files" list
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          ;; the rsync command
          (ptrv/rsync-command
           "rsync -arvz --progress "))
      ;; add all selected file names as arguments
      ;; to the rsync command
      (dolist (file files)
        (setq ptrv/rsync-command
              (concat ptrv/rsync-command
                      (shell-quote-argument file)
                      " ")))
      ;; append the destination
      (setq ptrv/rsync-command
            (concat ptrv/rsync-command
                    (shell-quote-argument dest)))
      ;; run the async shell command
      (async-shell-command ptrv/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1)))
  (bind-key "Y" #'ptrv/dired-rsync dired-mode-map)
  (defun ptrv/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "No more than 2 files should be marked"))))
  (bind-key "e" #'ptrv/ediff-files dired-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * projectile
(use-package projectile
  :ensure t
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  (dolist (file '(".ropeproject" "setup.py"))
    (add-to-list 'projectile-project-root-files file t))
  (run-with-idle-timer 10 nil (lambda () (projectile-cleanup-known-projects)))
  (setq projectile-completion-system 'helm)
  :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * find-file
(use-package find-file
  :bind ("C-c j o" . ff-find-other-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * processing
(use-package processing-mode
  :load-path "site-lisp/processing2-emacs"
  :mode ("\\.pde$" . processing-mode)
  :commands (processing-find-sketch)
  :config
  (use-package processing-snippets
    :load-path "site-lisp/processing2-emacs"
    :commands (processing-snippets-initialize)
    :after yasnippet
    :init (processing-snippets-initialize))

  (use-package processing-company
    :load-path "site-lisp/processing2-emacs"
    :commands (processing-company-setup)
    :after company
    :init (processing-company-setup))

  (bind-keys :map processing-mode-map
             ("C-c C-c" . processing-sketch-run)
             ("C-c C-d" . processing-find-in-reference)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * flycheck
(use-package flycheck
  :ensure t
  :bind ("C-c l e" . ptrv/flycheck-errors/body)
  :commands (flycheck-get-checker-for-buffer
             flycheck-may-enable-mode)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (defhydra ptrv/flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message"))
  (defun ptrv/flycheck-mode-on-safe ()
    (when (and (flycheck-may-enable-mode)
               (flycheck-get-checker-for-buffer))
      (flycheck-mode)))
  (advice-add 'flycheck-mode-on-safe :override
              #'ptrv/flycheck-mode-on-safe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * hideshow
(use-package hideshow
  :bind (("C-c b h"   . hs-toggle-hiding)
         ("C-c b H" . hs-toggle-hiding-all))
  :init (add-hook 'prog-mode-hook #'hs-minor-mode)
  :config
  ;; https://github.com/Hawstein/my-emacs/blob/master/_emacs/hs-minor-mode-settings.el
  (setq hs-isearch-open t)

  (defvar hs-hide-all nil
    "Current state of hideshow for toggling all.")

  (defun hs-toggle-hiding-all ()
    "Toggle hideshow all."
    (interactive)
    (setq hs-hide-all (not hs-hide-all))
    (if hs-hide-all
        (hs-hide-all)
      (hs-show-all)))
  :diminish hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * diminish
(defmacro ptrv/rename-modeline (package-name mode new-name)
  "Rename modeline for PACKAGE-NAME and MODE to NEW-NAME."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after ptrv/rename-modeline activate)
        (setq mode-name ,new-name))))

(ptrv/rename-modeline "clojure-mode" clojure-mode "λ")
(ptrv/rename-modeline "python" python-mode "Py")
(ptrv/rename-modeline "lisp-mode" emacs-lisp-mode "EL")
(ptrv/rename-modeline "markdown-mode" markdown-mode "md")
(ptrv/rename-modeline "processing-mode" processing-mode "P5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * osx
(use-package ns-win
  :if *is-mac*
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper
        ;; mac-right-command-modifier 'super
        mac-right-option-modifier nil))

(use-package ptrv-osx
  :load-path "site-lisp"
  :if *is-mac*
  :config
  (setq default-input-method "MacOSX")

  ;; Make cut and paste work with the OS X clipboard
  (when (not window-system)
    (setq interprogram-cut-function 'ptrv/paste-to-osx)
    (setq interprogram-paste-function 'ptrv/copy-from-osx))

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  ;; Ignore .DS_Store files with ido mode
  (with-eval-after-load 'ido
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;;GNU ls and find
(use-package files
  :config
  (when *is-mac*
    (let ((gnu-ls (executable-find "gls")))
      (if gnu-ls
          (setq insert-directory-program gnu-ls)
        (message "GNU coreutils not found. Install coreutils with homebrew.")))))

(use-package grep
  :config
  (when *is-mac*
    (let ((gnu-find (executable-find "gfind")))
      (when gnu-find
        (setq find-program gnu-find)))
    (let ((gnu-xargs (executable-find "gxargs")))
      (when gnu-xargs
        (setq xargs-program gnu-xargs)))))

(use-package locate
  :config
  (when *is-mac*
    (let ((mdfind (executable-find "mdfind")))
      (when mdfind
        (setq locate-command mdfind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * sclang
(use-package ptrv-sclang
  :load-path "site-lisp"
  :commands (ptrv/sclang-start)
  :mode ("\\.\\(sc\\|scd\\)$" . ptrv/sclang-mode-loader))

(use-package sclang
  :config
  (require 'ptrv-sclang)
  (ptrv/sclang-mode-loader--remove)

  (setq sclang-auto-scroll-post-buffer nil
        sclang-eval-line-forward nil
        ;;sclang-help-path '("~/.local/share/SuperCollider/Help")
        sclang-library-configuration-file "~/.sclang.cfg"
        sclang-runtime-directory "~/scwork/"
        sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")

  (add-hook 'sclang-mode-hook #'subword-mode)
  (bind-keys :map sclang-mode-map
             ("C-c ]"      . sclang-pop-definition-mark)
             ("s-."        . sclang-main-stop)
             ("<s-return>" . sclang-eval-region-or-line))

  (use-package company-sclang
    :load-path "site-lisp/company-sclang"
    :commands (company-sclang-setup)
    :after company
    :init (company-sclang-setup)
    :config (unbind-key "C-M-i" sclang-mode-map))

  (use-package sclang-snippets
    :load-path "site-lisp/sclang-snippets"
    :commands (sclang-snippets-initialize)
    :after yasnippet
    :init (sclang-snippets-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * python
(use-package python
  :init
  (with-eval-after-load 'flycheck
    (defun ptrv/python-mode-flycheck-config ()
      (setq-local flycheck-check-syntax-automatically
                  (delq 'idle-change
                        flycheck-check-syntax-automatically)))
    (add-hook 'python-mode-hook
              #'ptrv/python-mode-flycheck-config))
  (defun ptrv/pyenv-mode-set-maybe ()
    "Automatically activates pyenv version if .python-version file exists."
    (when (or (featurep 'pyenv-mode) (require 'pyenv-mode nil t))
      (-when-let (pyenv-version-file
                  (catch 'found
                    (f-traverse-upwards
                     (lambda (path)
                       (let ((f (f-expand ".python-version" path)))
                         (when (f-exists? f)
                           (throw 'found f)))))))
        (pyenv-mode-set (s-trim (f-read-text pyenv-version-file 'utf-8))))))
  (add-hook 'find-file-hook #'ptrv/pyenv-mode-set-maybe)
  :config
  (setq python-check-command "flake8")

  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))

  ;; (let ((ipython (executable-find "ipython")))
  ;;   (if ipython
  ;;       (setq python-shell-interpreter "ipython")
  ;;     (warn "IPython is missing, falling back to default python")))

  (info-lookup-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z_0-9.]+"
   :doc-spec
   '(("(python)Python Module Index" )
     ("(python)Index"
      (lambda
        (item)
        (cond
         ((string-match
           "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
          (format "%s.%s" (match-string 2 item)
                  (match-string 1 item))))))))

  (use-package anaconda-mode
    :disabled t
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode))

  (use-package company-anaconda
    :disabled t
    :ensure t
    :after company
    :init
    (defun ptrv/company-anaconda--init ()
      (setq-local company-backends
                  '((company-anaconda :with company-yasnippet))))
    (add-hook 'python-mode-hook 'ptrv/company-anaconda--init))

  (use-package highlight-indentation
    :ensure t
    :init (add-hook 'python-mode-hook 'highlight-indentation-mode))

  (use-package pyenv-mode
    :ensure t
    :config
    (pyenv-mode)
    (unbind-key "C-c C-s" pyenv-mode-map)
    (unbind-key "C-c C-u" pyenv-mode-map)
    (bind-keys :map pyenv-mode-map
               ("C-. p s" . pyenv-mode-set)
               ("C-. p u" . pyenv-mode-unset)))

  (use-package pytest
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * cc-mode
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode)
         ("\\.i\\(nl\\|pp\\)\\'"      . c++-mode))
  :init
  (defun ptrv/cc-mode-init ()
    (c-set-style "my-cc-mode")
    (setq c-basic-offset 4
          tab-width 4
          ;;c-indent-level 4
          c-default-style "bsd"
          indent-tabs-mode nil)
    (setq-local split-width-threshold nil))
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'ptrv/cc-mode-init))
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "bsd")))
  (c-add-style "my-cc-mode"
               '("bsd"
                 (c-basic-offset . 4)
                 (c-offsets-alist . ((innamespace . 0)))))

  ;; doxymacs
  (use-package doxymacs
    :commands (doxymacs-mode)
    :config
    (defvar doxymacs-external-xml-parser-executable)
    (unless (file-exists-p doxymacs-external-xml-parser-executable)
      (warn "The doxymacs_parser executable does not exist!"))
    (defvar doxymacs-mode)
    (add-hook 'font-lock-mode-hook
              (lambda ()
                (when (and doxymacs-mode
                           (or (eq major-mode 'c-mode)
                               (eq major-mode 'c++-mode)))
                  (doxymacs-font-lock)))))

  (use-package xcscope
    :disabled t
    :ensure t
    :init
    (dolist (mode-hook '(c++-mode-hook c-mode-hook))
      (add-hook mode-hook #'cscope-minor-mode)))

  (use-package clang-format
    :ensure t
    :bind ("C-. f" . clang-format-region))

  (use-package helm-cscope
    :ensure t
    :bind
    (:map helm-cscope-mode-map
          ("C-c , s" . helm-cscope-find-this-symbol)
          ("C-c , g" . helm-cscope-find-global-definition)
          ("C-c , C" . helm-cscope-find-called-function)
          ("C-c , c" . helm-cscope-find-calling-this-funtcion)
          ("C-c , t" . helm-cscope-find-this-text-string)
          ("C-c , e" . helm-cscope-find-egrep-pattern)
          ("C-c , f" . helm-cscope-find-this-file)
          ("C-c , i" . helm-cscope-find-files-including-file)
          ("C-c , =" . helm-cscope-find-assignments-to-this-symbol))
    :init
    (dolist (hook '(c++-mode-hook c-mode-hook))
      (add-hook hook #'helm-cscope-mode))
    :diminish helm-cscope-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * gud
(use-package gud
  :commands gud-gdb
  :init
  (defun ptrv/show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*gud-" (buffer-name buf))
                 (throw 'found buf))))))
      (if gud-buf
          (switch-to-buffer gud-buf)
        (call-interactively (if *is-mac* 'lldb 'gud-gdb)))))
  (bind-key "C-. g" #'ptrv/show-debugger)
  :config
  (bind-keys ("<f9>"    . gud-cont)
             ("<f10>"   . gud-next)
             ("<f11>"   . gud-step)
             ("S-<f11>" . gud-finish)))

(use-package gud-lldb
  :if *is-mac*
  :load-path "site-lisp"
  :commands (lldb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ycmd
(use-package ycmd
  :load-path "~/src/emacs-ycmd"
  :commands (ycmd-mode)
  :init
  (dolist (hook '(c-mode-hook
                  c++-mode-hook
                  go-mode-hook
                  python-mode-hook
                  rust-mode-hook
                  js-mode-hook
                  typescript-mode-hook
                  csharp-mode-hook))
    (add-hook hook #'ycmd-mode))
  :config
  (defun ptrv/ycmd-show-server-buffer ()
    (interactive)
    (-if-let (buf (get-buffer ycmd--server-buffer-name))
        (switch-to-buffer buf)
      (error "No YCMD server buffer")))
  (bind-key "w" #'ptrv/ycmd-show-server-buffer ycmd-command-map)
  (bind-key "M-." #'ycmd-goto ycmd-mode-map)
  (bind-key "M-?" #'ycmd-goto-definition ycmd-mode-map)
  (use-package company-ycmd
    :commands (company-ycmd-setup)
    :after company
    :init (company-ycmd-setup))

  (use-package flycheck-ycmd
    :commands (flycheck-ycmd-setup)
    :after flycheck
    :init (flycheck-ycmd-setup))

  (use-package ycmd-eldoc
    :disabled t
    :commands (ycmd-eldoc-setup)
    :after eldoc
    :init
    (add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup))

  (defun ptrv/company-ycmd-complete (arg)
    (interactive "P")
    (let ((ycmd-force-semantic-completion (not arg)))
      (company-cancel)
      (company-complete)))
  (bind-key [remap complete-symbol]
            #'ptrv/company-ycmd-complete ycmd-mode-map)
  (defun ptrv/ycmd-after-exception-try-cscope-or-ag (type buf pos _res)
    (when (equal type "GoToDefinition")
      (save-excursion
        (with-current-buffer buf
          (goto-char pos)
          (ycmd--save-marker)
          (if (file-exists-p (expand-file-name
                              cscope-index-file (projectile-project-root)))
              (cscope-find-global-definition-no-prompting)
            (ag (symbol-name (symbol-at-point))
                (file-name-directory (buffer-file-name))))))))
  (add-hook 'ycmd-after-exception-hook
            #'ptrv/ycmd-after-exception-try-cscope-or-ag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * fill-column-indicator
(use-package fill-column-indicator
  :disabled t
  :ensure t
  :commands (fci-mode)
  :init
  (add-hook 'c-mode-hook 'fci-mode)
  (add-hook 'c++-mode-hook 'fci-mode)
  :config
  ;; 120 char of limit
  (setq fci-rule-column 120)

  ;; workaround to avoid intereference with company
  (defun on-off-fci-before-company(command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))

  (advice-add 'company-call-frontends
              :before #'on-off-fci-before-company))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * rtags
(use-package rtags
  :disabled t
  :load-path "~/src/rtags/src"
  :commands (rtags-start-process-unless-running)
  :init
  (defun ptrv/rtags-init()
    (local-set-key (kbd "M-.") #'rtags-find-symbol-at-point)
    (local-set-key (kbd "M-,") #'rtags-location-stack-back))
  (dolist (it '(c++-mode-hook c-mode-hook))
    (add-hook it #'rtags-start-process-unless-running)
    (add-hook it #'ptrv/rtags-init))
  :config
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t)
  (rtags-diagnostics)
  (use-package company-rtags
    :after company
    :init
    (add-to-list 'company-backends 'company-rtags))
  (use-package flycheck-rtags
    :after flycheck
    :init
    (defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil))
    ;; c-mode-common-hook is also called by c++-mode
    (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * lua
(use-package lua-mode
  :ensure t
  :config
  (defun ptrv/lua-send-region-or-current-line ()
    "Send current region or line to lua process."
    (interactive)
    (if (region-active-p)
        (lua-send-region (region-beginning) (region-end))
      (lua-send-current-line)))
  (bind-keys :map lua-mode-map
             ("C-c C-d" . lua-send-proc)
             ("C-c C-c" . ptrv/lua-send-region-or-current-line)
             ("C-c C-p" . lua-start-process))
  (use-package company-lua
    :load-path "site-lisp/company-lua"
    :after company
    :init
    (defun ptrv/lua-mode-company-init ()
      (setq-local company-backends '((company-lua
                                      company-etags
                                      company-dabbrev-code
                                      company-yasnippet))))
    (add-hook 'lua-mode-hook #'ptrv/lua-mode-company-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * html
;; (use-package sgml-mode
;;   :config
;;   (require 'smartparens-html)
;;   (add-to-list 'sp-navigate-consider-stringlike-sexp 'html-mode)
;;   (bind-keys :map html-mode-map
;;              ("C-c C-f" . sp-html-next-tag)
;;              ("C-c C-b" . sp-html-previous-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * turtle
(use-package "ttl-mode"
  :mode (("\\.ttl$" . ttl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->"       . mc/mark-next-like-this)
         ("C-<"       . mc/mark-previous-like-this)
         ("C-c u n"   . mc/skip-to-next-like-this)
         ("C-c u p"   . mc/skip-to-previous-like-this)
         ("C-c u N"   . mc/unmark-next-like-this)
         ("C-c u P"   . mc/unmark-previous-like-this)
         ("C-c u m"   . mc/mark-all-dwim)
         ("C-c u M"   . mc/mark-more-like-this-extended)
         ("C-c u i"   . mc/insert-numbers)
         ("C-c u h"   . mc-hide-unmatched-lines-mode)
         ("C-c u a"   . mc/mark-all-like-this)
         ("C-c u d"   . mc/mark-all-symbols-like-this-in-defun)
         ("C-c u r"   . mc/reverse-regions)
         ("C-c u s"   . mc/sort-regions)
         ("C-c u l"   . mc/edit-lines)
         ("C-c u C-a" . mc/edit-beginnings-of-lines)
         ("C-c u C-e" . mc/edit-ends-of-lines)
         ("C-c u M"   . mc/mark-more-like-this-extended)
         ("C-c u SPC" . set-rectangular-region-anchor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * key-chord
(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config
  (key-chord-define-global "BB" 'ido-switch-buffer)
  (key-chord-define-global "JJ" (lambda ()
                                  (interactive)
                                  (switch-to-buffer
                                   (other-buffer (current-buffer) 1))))
  (with-eval-after-load 'cc-mode
    (key-chord-define c++-mode-map ";;" "\C-e;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * avy
(use-package avy
  :ensure t
  :bind (("C-o" . avy-goto-word-or-subword-1)
         ("C-O" . avy-goto-char))
  :config
  (setq avy-background t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ace-jump-mode
(use-package ace-jump-mode
  :disabled t
  :ensure t
  :bind ("C-o" . ace-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * browse-kill-ring
(use-package browse-kill-ring
  :disabled t
  :ensure t
  :bind ("M-C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-show-preview nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * find-func
(use-package find-func
  :init (find-function-setup-keys)
  :bind (;; Help should search more than just commands
         ("C-c h f" . find-function)
         ("C-c h k" . find-function-on-key)
         ("C-c h v" . find-variable)
         ("C-c h l" . find-library)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * windows
(use-package window
  :bind (("C-c w ="  . balance-windows)
         ("C-c w /"  . split-window-right)
         ("C-c w -"  . split-window-below)
         ("C-c w k"  . delete-window)
         ("C-c w z" . delete-other-windows)
         ("C-<return>". other-window))
  :init
  (bind-key "C-c w ." (lambda () (interactive) (shrink-window-horizontally 4)))
  (bind-key "C-c w ," (lambda () (interactive) (enlarge-window-horizontally 4)))
  (bind-key "C-c w <down>" (lambda () (interactive) (enlarge-window -4)))
  (bind-key "C-c w <up>" (lambda () (interactive) (enlarge-window 4)))
  ;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
  (bind-key "C-x O" (lambda () (interactive) (other-window -1))))

(use-package ptrv-window
  :load-path "site-lisp"
  :bind (("C-c w s" . ptrv/swap-windows)
         ("C-c w r" . ptrv/rotate-windows)
         ("C-c w v" . ptrv/halve-other-window-height-or-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ace-window
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * golden-ratio
(use-package golden-ratio
  :ensure t
  :init
  (defun ptrv/toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . ptrv/toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window)
        ;; Exclude a couple of special modes from golden ratio, namely
        ;; Flycheck's error list, calc
        golden-ratio-exclude-modes '(flycheck-error-list-mode
                                     calc-mode
                                     dired-mode
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * buffers

(use-package ptrv-buffers
  :load-path "site-lisp"
  :commands (ptrv/do-not-kill-important-buffers)
  :init
  (add-hook 'kill-buffer-query-functions
            #'ptrv/do-not-kill-important-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * align
(use-package align
  :bind (("C-c x a a" . align)
         ("C-c x a c" . align-current)
         ("C-c x a r" . align-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * simple
(use-package ptrv-simple
  :load-path "site-lisp"
  :commands (lorem)
  :bind (([remap goto-line] . ptrv/goto-line-with-feedback)
         ("<S-return>" . ptrv/smart-open-line)
         ("M-o" . ptrv/smart-open-line)
         ("<C-S-return>" . ptrv/smart-open-line-above)
         ("C-M-=" . ptrv/increment-number-at-point)
         ("C-M--" . ptrv/decrement-number-at-point)
         ("C-c d" . ptrv/duplicate-current-line-or-region)
         ("C-c M-d" . ptrv/duplicate-and-comment-current-line-or-region)
         ("C-c a b" . ptrv/browse-url)
         ("C-c q" . ptrv/exit-emacs-client)
         ("M-;" . ptrv/comment-dwim-line)
         ([remap move-beginning-of-line] . ptrv/smarter-move-beginning-of-line)
         ;; ("C-M-\\" . ptrv/indent-region-or-buffer)
         ("C-c x i" . ptrv/indent-region-or-buffer)
         ("C-M-z" . ptrv/indent-defun)
         ("C-c i d" . ptrv/insert-current-date)
         ("C-c f S" . sudo-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * bool-flip
(use-package bool-flip
  :ensure t
  :bind ("C-c x b" . bool-flip-do-flip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * docs
(use-package zeal-at-point
  :if *is-linux*
  :ensure t
  :bind ("C-c h d" . zeal-at-point))

(use-package dash-at-point
  :if *is-mac*
  :ensure t
  :bind ("C-c h d" . dash-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * server
(use-package server
  :if window-system
  :commands (server-running-p server-start)
  :init (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * welcome-message stuff
(defun ptrv/user-first-name ()
  "Get user's first name."
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))
(defun ptrv/user-first-name-p ()
  "Check whether the user name is provided."
  (not (string-equal "" (ptrv/user-first-name))))

(defvar ptrv/welcome-messages
  (if (ptrv/user-first-name-p)
      (list (concat "Hello " (ptrv/user-first-name) ", somewhere in the world the sun is shining for you right now.")
            (concat "Hello " (ptrv/user-first-name) ", it's lovely to see you again. I do hope that you're well.")
            (concat (ptrv/user-first-name) ", turn your head towards the sun and the shadows will fall behind you."))
    (list  "Hello, somewhere in the world the sun is shining for you right now."
           "Hello, it's lovely to see you again. I do hope that you're well."
           "Turn your head towards the sun and the shadows will fall behind you.")))

(defun ptrv/welcome-message ()
  "Get random welcom message."
  (nth (random (length ptrv/welcome-messages)) ptrv/welcome-messages))

(setq initial-scratch-message (concat ";;
;; Emacs on " system-name " [" (symbol-name system-type) "]
;;
;; " (ptrv/welcome-message) "

"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; eval: (orgstruct-mode 1)
;; orgstruct-heading-prefix-regexp: ";;;; "
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; init.el ends here
