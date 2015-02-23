;;; init.el --- Emacs configuration of Peter Vasil
;;
;; Copyright (c) 2013-2014 Peter Vasil <mail@petervasil.net>
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

(defmacro ptrv/hook-into-modes (func modes)
  "Add FUNC to list of MODES."
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

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
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package paradox
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . paradox-list-packages-no-fetch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * PATH
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * custom settings
(defconst ptrv/custom-file (locate-user-emacs-file "custom.el"))
(use-package cus-edit
  :defer t
  :init (load ptrv/custom-file :no-error :no-message)
  :config (setq custom-file ptrv/custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ctl-period-map
(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * builtins
(setq-default fill-column 72
              indent-tabs-mode nil ; And force use of spaces
              c-basic-offset 4     ; indents 4 chars
              tab-width 4)         ; and 4 char wide for TAB

(defun ptrv/get-default-url-program ()
  "Get default program to handle urls."
  (cond
   (*is-mac* (executable-find "open"))
   (*is-linux* (executable-find "x-www-browser"))))

(use-package browse-url
  :defer t
  :config
  (setq browse-url-generic-program (ptrv/get-default-url-program)
        browse-url-browser-function 'browse-url-generic))

(use-package bookmark
  :bind ("C-c l b" . list-bookmarks)
  :config (setq bookmark-save-flag t))

(use-package simple
  :bind ("C-x p" . pop-to-mark-command)
  :config
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

(use-package minibuffer
  :defer t
  :config
  (setq completion-cycle-threshold 5))

(use-package apropos
  :defer t
  :bind (("C-c h a" . apropos)
         ("C-c h A" . apropos-command)))

(use-package autoinsert
  :init (auto-insert-mode))
(use-package jka-cmpr-hook
  :init (auto-compression-mode))
(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :init (winner-mode))

(use-package windmove
  :config (windmove-default-keybindings 'super))

(use-package recentf
  :defer t
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 100))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :config (setq-default save-place t))

(setq history-length 1000)
(use-package savehist
  :init (savehist-mode t)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-save-minibuffer-history t))

;; desktop.el
(use-package desktop
  :init (desktop-save-mode)
  :config (setq desktop-save 'if-exists))

(use-package whitespace
  :bind ("C-c T w" . whitespace-mode)
  :init (ptrv/hook-into-modes #'whitespace-mode
                              '(prog-mode-hook text-mode-hook))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing)
        whitespace-line-column nil)    ; Use `fill-column' for overlong lines
  :diminish whitespace-mode)

(use-package whitespace-cleanup-mode
  :ensure t
  :bind ("C-c T W" . whitespace-cleanup-mode)
  :init (ptrv/hook-into-modes #'whitespace-cleanup-mode
                              '(prog-mode-hook text-mode-hook))
  :config
  (setq whitespace-cleanup-mode-only-if-initially-clean t))

;; disabled commands
(setq disabled-command-function nil)

;;enable cua-mode for rectangular selections
(use-package cua-base
  :defer t
  :config (setq cua-enable-cua-keys nil))

(defun ptrv/get-default-sound-command ()
  "Get default command for playing sound files."
  (cond
   (*is-mac* (executable-find "afplay"))
   (*is-linux* (executable-find "paplay"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * compilation
;; Compilation from Emacs
(use-package compile
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :init
  (progn
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
    (bind-key "C-. c" #'ptrv/show-compilation))
  :config
  (progn
    (require 'ansi-color)
    (defun ptrv/colorize-compilation-buffer ()
      (ansi-color-process-output nil)
      (setq-local comint-last-output-start (point-marker)))
    (add-hook 'compilation-filter-hook
              #'ptrv/colorize-compilation-buffer)
    ;; other settings
    (setq compilation-scroll-output t
          compilation-always-kill t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * email
(setq user-full-name "Peter Vasil"
      user-mail-address "mail@petervasil.net")

(use-package message
  :defer t
  :config
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :defer t
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
;;;; * diff
(use-package diff
  :bind (("C-c D d" . diff)
         ("C-c D f" . diff-buffer-with-file))
  :config
  (setq diff-switches "-u"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ediff
(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * spelling
(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ;; ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "en"          ; default dictionary
        ispell-silently-savep t))       ; Don't ask when saving the private dict

(use-package flyspell
  :defer t
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)
    (bind-keys :map flyspell-mode-map
               ("\M-\t" . nil)
               ("C-:"   . flyspell-auto-correct-word)
               ("C-."   . ispell-word))))

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
  :bind ("C-c T r" . rainbow-mode)
  :diminish rainbow-mode)

(setq column-number-mode t)
(use-package hl-line
  :init (global-hl-line-mode))

(use-package zeburn
  :ensure zenburn-theme
  :defer t
  :init (load-theme 'zenburn :no-confirm))

(defvar todo-comment-face 'todo-comment-face)
(defvar headline-face 'headline-face)

;; Fontifying todo items outside of org-mode
(defface todo-comment-face
  '((t (:foreground "#cd0000"
        :weight bold
        :bold t)))
  "Face for TODO in code buffers."
  :group 'org-faces)
(defface headline-face
  '((t (:inherit font-lock-comment-face
        :weight bold
        :bold t
        :underline t)))
  "Face for headlines."
  :group 'org-faces)

(defun fontify-todo ()
  "Fontify todos."
  (font-lock-add-keywords
   nil `(("\\<\\(FIX\\(ME\\)?\\|TODO\\)"
          1 todo-comment-face t))))

(add-hook 'prog-mode-hook 'fontify-todo)

(defun fontify-headline ()
  "Fontify certain headlines."
  (font-lock-add-keywords
   nil '(("^;;;; [* ]*\\(.*\\)\\>"
          (1 headline-face t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ido
(use-package ido
  :init (ido-mode)
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
  :ensure t
  :init (ido-ubiquitous-mode)
  :config
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
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * idomenu
(use-package idomenu
  :ensure t
  :bind (("C-x C-i" . idomenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Eshell
(use-package eshell
  :bind (("C-x m" . eshell)
         ("C-. t" . ptrv/eshell-or-restore))
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
  (progn
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
      :load-path "site-lisp/pcomplete-plugins")

    (use-package pcmpl-git
      :ensure t)

    (use-package starter-kit-eshell
      :ensure t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * company
(use-package company
  :ensure t
  :defer t
  :idle (global-company-mode)
  :config
  (progn
    (setq company-idle-delay 0.5
          company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-show-numbers t
          company-global-modes '(not magit-status-mode)
          ;; company-transformers '(company-sort-by-occurrence))
          )
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (bind-key [remap complete-symbol] #'company-complete company-mode-map)))

;; (use-package company-statistics
;;   :ensure t
;;   :defer t
;;   :init (company-statistics-mode))

(use-package company-dabbrev
  :ensure company
  :defer t
  :config (setq company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (company-quickhelp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * smartparens
(use-package smartparens
  :ensure t
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-global-mode))
  :config
  (progn
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (dolist (it sp-paredit-bindings)
      (bind-key (car it) (cdr it) smartparens-strict-mode-map))

    (bind-key "M-q" #'sp-indent-defun smartparens-strict-mode-map)
    (bind-key "C-j" #'sp-newline smartparens-strict-mode-map)
    (bind-key "M-?" #'sp-convolute-sexp smartparens-strict-mode-map)

    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "M-("))

    (dolist (mode sp--lisp-modes)
      (let ((hook (intern (format "%s-hook" (symbol-name mode)))))
        (add-hook hook 'smartparens-strict-mode)))

    (add-hook 'eval-expression-minibuffer-setup-hook
              'smartparens-strict-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * elisp
(use-package lisp-mode
  :defer t
  :mode (("\\.el$" . emacs-lisp-mode)
         ("/Cask$" . emacs-lisp-mode))
  :config
  (progn
    (with-eval-after-load 'company
      (defun ptrv/company-elisp--init ()
        (setq-local company-backends '((company-capf :with company-dabbrev))))
      (add-hook 'emacs-lisp-mode-hook 'ptrv/company-elisp--init))

    (defun imenu-elisp-sections ()
      "Add custom expression to imenu."
      (setq imenu-prev-index-position-function nil)
      (add-to-list 'imenu-generic-expression '("Sections" "^;;;; [* ]*\\(.+\\)$" 1) t))
    (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

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
      :defer t
      :init
      (progn
        (defun ptrv/switch-to-ielm ()
          (interactive)
          (pop-to-buffer (get-buffer-create "*ielm*"))
          (ielm))
        (bind-key "C-c C-z" 'ptrv/switch-to-ielm emacs-lisp-mode-map)))

    (bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)
    (bind-key "C-c C-e" 'eval-and-replace lisp-mode-shared-map)
    (bind-key "C-c C-p" 'eval-print-last-sexp lisp-mode-shared-map)
    (bind-key "M-RET" 'ptrv/lisp-describe-thing-at-point lisp-mode-shared-map)

    (use-package lexbind-mode
      :ensure t
      :init (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))))

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :init
  (ptrv/hook-into-modes #'elisp-slime-nav-mode
                        '(emacs-lisp-mode-hook ielm-mode-hook))
  :diminish elisp-slime-nav-mode)

(use-package eldoc
  :defer t
  :init (ptrv/hook-into-modes
         #'eldoc-mode '(emacs-lisp-mode-hook
                        lisp-interaction-mode-hook
                        ielm-mode-hook
                        eval-expression-minibuffer-setup-hook))
  :diminish eldoc-mode)

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (ptrv/hook-into-modes #'rainbow-delimiters-mode
                        '(text-mode-hook prog-mode-hook)))

;; (use-package nrepl-eval-sexp-fu
;;   :ensure t
;;   :init (require 'nrepl-eval-sexp-fu)
;;   :config
;;   (setq nrepl-eval-sexp-fu-flash-duration 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * clojure
;; (ptrv/after find-file-in-project
;;   (add-to-list 'ffip-patterns "*.clj"))

;; (ptrv/after clojure-mode
;;   (message "clojure config has been loaded !!!")

;;   (ptrv/add-to-hook 'clojure-mode-hook ptrv/lisp-common-modes)

;;   (ptrv/smartparens-setup-lisp-modes '(clojure-mode))

;;   (defun ptrv/clojure-mode-init ()
;;     (yas-minor-mode 1))
;;   (add-hook 'clojure-mode-hook 'ptrv/clojure-mode-init))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; * nrepl
;; (add-to-list 'same-window-buffer-names "*cider*")

;; (ptrv/after cider-repl
;;   (setq cider-repl-pop-to-buffer-on-connect t)
;;   (setq cider-repl-popup-stacktraces t))

;; (ptrv/after nrepl-client
;;   ;; (setq nrepl-port "4555")
;;   (setq nrepl-buffer-name-show-port t))

;; (ptrv/after cider-interaction
;;   (setq cider-show-error-buffer nil))

;; (ptrv/after cider-repl-mode
;;   (define-key cider-repl-mode-map (kbd "M-RET") 'cider-doc)
;;   (ptrv/smartparens-setup-lisp-modes '(cider-repl-mode)))

;; (ptrv/after cider-mode
;;   (message "cider-mode config has been loaded!!!")

;;   (ptrv/hook-into-modes 'cider-turn-on-eldoc-mode
;;                         '(cider-mode))

;;   ;; Show documentation/information with M-RET
;;   (define-key cider-mode-map (kbd "M-RET") 'cider-doc))

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
  :defer t
  :config
  (setq tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(defun sudo-edit (&optional arg)
  "Edit buffer with superuser privileges.
If ARG is non-nil prompt for filename."
  (interactive "P")
  (let (auth-sources)
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ibuffer
(use-package ibuf-ext
  :defer t
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("default"
           ("IRC"      (mode . erc-mode))
           ("emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Completions\\*$")
                        (filename . ".emacs.d")
                        (filename . ".live-packs")))
           ("magit" (name . "\\*magit"))
           ("dired" (mode . dired-mode))
           ("sclang" (mode . sclang-mode))
           ("Org" (mode . org-mode))
           ("Help" (or (name . "\\*Help\\*")
                       (name . "\\*Apropos\\*")
                       (name . "\\*info\\*")))
           ("#!-config" (filename . ".cb-config"))
           ("ssh" (filename . "^/ssh.*"))
           ("root" (filename . "^/sudo:root.*"))))))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :init (add-hook 'ibuffer-mode-hook
                  (lambda ()
                    (ibuffer-auto-mode 1)
                    (ibuffer-switch-to-saved-filter-groups "default"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * gist
(use-package yagist
  :ensure t
  :bind(("C-c G c" . yagist-region-or-buffer)
        ("C-c G p" . yagist-region-or-buffer-private)
        ("C-c G l" . yagist-list))
  :config (setq yagist-view-gist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (add-hook 'magit-log-edit-mode-hook
              (lambda ()
                (set-fill-column 72)
                (auto-fill-mode 1)))

    ;; http://whattheemacsd.com/setup-magit.el-01.html
    ;; full screen magit-status
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defun magit-quit-session (&optional kill-buffer)
      "Restores the previous window configuration and kills the magit buffer"
      (interactive "P")
      (if kill-buffer
          (kill-buffer)
        (bury-buffer))
      (jump-to-register :magit-fullscreen))

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (bind-keys :map magit-status-mode-map
               ("q" . magit-quit-session)
               ("Q" . (lambda () (interactive) (magit-quit-session t)))
               ("W" . magit-toggle-whitespace))

    (setq magit-auto-revert-mode nil
          magit-set-upstream-on-push t
          magit-completing-read-function 'magit-ido-completing-read)))

(use-package git-commit-mode
  :ensure t
  :defer t
  :init (add-hook 'git-commit-mode-hook
                  (lambda ()
                    (set-fill-column 72)
                    (auto-fill-mode 1))))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package git-rebase-mode
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * vc
(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks)) t
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * vc-git
(when (eval-when-compile (version-list-<
                          (version-to-list emacs-version)
                          '(25 0 50 0)))
  (message "Some vc-git functions from Emacs 25 have been defined...")
  (defun vc-git-conflicted-files (directory)
    "Return the list of files with conflicts in DIRECTORY."
    (let* ((status
            (vc-git--run-command-string directory "status" "--porcelain" "--"))
           (lines (split-string status "\n" 'omit-nulls))
           files)
      (dolist (line lines files)
        (when (string-match "\\([ MADRCU?!][ MADRCU?!]\\) \\(.+\\)\\(?: -> \\(.+\\)\\)?"
                            line)
          (let ((state (match-string 1 line))
                (file (match-string 2 line)))
            ;; See git-status(1).
            (when (member state '("AU" "UD" "UA" ;; "DD"
                                  "DU" "AA" "UU"))
              (push file files)))))))

  (defun vc-git-resolve-when-done ()
    "Call \"git add\" if the conflict markers have been removed."
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^<<<<<<< " nil t)
        (vc-git-command nil 0 buffer-file-name "add")
        ;; Remove the hook so that it is not called multiple times.
        (remove-hook 'after-save-hook 'vc-git-resolve-when-done t))))

  (defun vc-git-find-file-hook ()
    "Activate `smerge-mode' if there is a conflict."
    (when (and buffer-file-name
               (vc-git-conflicted-files buffer-file-name)
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^<<<<<<< " nil 'noerror)))
      (vc-file-setprop buffer-file-name 'vc-state 'conflict)
      (smerge-start-session)
      (add-hook 'after-save-hook 'vc-git-resolve-when-done nil 'local)
      (message "There are unresolved conflicts in this file"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * git-messanger
(use-package git-messenger
  :ensure t
  :defer t
  :bind (("C-x v p" . git-messenger:popup-message))
  :config (setq git-messenger:show-detail t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * diff-hl
(use-package diff-hl
  :ensure t
  :defer t
  :init
  (progn
    (global-diff-hl-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

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
  :defer t
  :mode ("\\.yasnippet$" . yasnippet-mode)
  :config
  (progn
    (setq yas-prompt-functions '(yas-x-prompt
                                 yas-ido-prompt
                                 yas-completing-prompt))

    (unless yas-global-mode (yas-reload-all))

    (use-package dropdown-list
      :ensure t
      :defer t
      :init
      (add-to-list 'yas-prompt-functions 'yas-dropdown-prompt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * undo-tree
(use-package undo-tree
  :ensure t
  :defer t
  :idle (global-undo-tree-mode)
  :idle-priority 1
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pomodoro.el
(use-package pomodoro
  :ensure t
  :defer t
  :config
  (progn
    (pomodoro-add-to-mode-line)
    (setq pomodoro-sound-player (ptrv/get-default-sound-command)
          pomodoro-break-start-sound (expand-file-name "sounds/alarm.wav" ptrv/etc-dir)
          pomodoro-work-start-sound (expand-file-name "sounds/alarm.wav" ptrv/etc-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * sql-mode
(use-package sql
  :defer t
  :config
  (bind-keys :map sql-mode-map
             ("C-c C-p p" . sql-set-product)
             ("C-c C-p i" . sql-set-sqli-buffer)))

(use-package sql-spatialite-ext
  :defer t
  :commands (sql-spatialite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * tea-time
(use-package tea-time
  :ensure t
  :defer t
  :commands (tea-time)
  :config
  (setq tea-time-sound (expand-file-name "sounds/alarm.wav" ptrv/etc-dir)
        tea-time-sound-command (concat
                                (ptrv/get-default-sound-command) " %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * frames
;; display visited file's path as frame title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;; defalias
(defalias 'toggle-fullscreen 'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * iflipb
(use-package iflipb
  :ensure t
  :defer t
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
          "*scratch*"
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
(bind-key "C-c s" search-map)
(bind-key "O" 'multi-occur search-map)

;; the silver searcher
(use-package ag
  :ensure t
  :defer t
  :bind(("C-c a a" . ag)
        ("C-c a A" . ag-regexp)
        ("C-c a d" . ag-dired-regexp)
        ("C-c a D" . ag-dired)
        ("C-c a f" . ag-files)
        ("C-c a k" . ag-kill-other-buffers)
        ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * edit-server
(use-package edit-server
  :ensure t
  :defer t
  :idle (edit-server-start)
  :idle-priority 10
  :init
  (progn
    (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
    (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer))
  :config
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))
        edit-server-new-frame nil))

(use-package edit-server-htmlize
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * iedit
(use-package iedit
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * google-this
(use-package google-this
  :ensure t
  :defer t
  :init (google-this-mode)
  :diminish google-this-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * popwin
(use-package popwin
  :ensure t
  :init (popwin-mode)
  :config
  (progn
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
            ("*nrepl-error*" :height ,(ptrv/get-popwin-height 'medium) :stick t)
            ("*nrepl-doc*" :height ,(ptrv/get-popwin-height 'medium) :stick t)
            ("*nrepl-src*" :height ,(ptrv/get-popwin-height 'medium) :stick t)
            ("\\*nrepl " :regexp t :height ,(ptrv/get-popwin-height 'medium) :stick t)
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
            ("*company-documentation*" :noselect t :height ,(ptrv/get-popwin-height 'small))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * buffer
(use-package autorevert
  :init (global-auto-revert-mode)
  :config
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
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l a" . org-agenda)
         ("C-c l b" . org-iswitchb)
         ("C-c l c" . org-capture)
         ("C-c l o" . org-store-link))
  :config
  (progn
    (setq org-outline-path-complete-in-steps nil
          org-completion-use-iswitchb nil
          org-completion-use-ido t
          org-log-done t
          org-src-fontify-natively nil
          ;; Set agenda files in custom.el or use default
          ;; org-directory "~/Dropbox/org"
          org-default-notes-file (expand-file-name "captures.org" org-directory)
          ;; org-agenda-files `(,(expand-file-name "ptrv.org" org-directory))
          org-link-mailto-program '(browse-url "https://mail.google.com/mail/?view=cm&to=%a&su=%s")
          )

    (with-eval-after-load 'yasnippet
      (defun yas-org-very-safe-expand ()
        (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

      (defun org-mode-yasnippet-workaround ()
        (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand))
      (add-hook 'org-mode-hook 'org-mode-yasnippet-workaround)

      (defun org-mode-init ()
        (turn-off-flyspell))
      (add-hook 'org-mode-hook 'org-mode-init)

      (bind-key "C-c g" 'org-sparse-tree org-mode-map))))

(use-package org-clock
  :ensure org
  :defer t
  :config (setq org-clock-into-drawer t))

(use-package org-mobile
  :ensure org
  :defer t
  :config
  (setq org-mobile-directory "~/Dropbox/MobileOrg"
        org-mobile-files '("~/Dropbox/org/ptrv.org"
                           "~/Dropbox/org/notes.org"
                           "~/Dropbox/org/journal.org")
        org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org"))

(use-package org-agenda
  :ensure org
  :defer t
  :config
  (setq org-agenda-custom-commands
        '(("P" "Projects"
           ((tags "PROJECT")))
          ("H" "Home Lists"
           ((tags "HOME")
            (tags "COMPUTER")
            (tags "DVD")
            (tags "READING")))
          ("W" "Work Lists"
           ((tags "WORK")))
          ("D" "Daily Action List"
           ((agenda "" ((org-agenda-ndays 1)
                        (org-agenda-sorting-strategy
                         '((agenda time-up priority-down tag-up)))
                        (org-deadline-warning-days 0))))))))

(use-package org-capture
  :ensure org
  :defer t
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (expand-file-name "ptrv.org" org-directory) "TASKS")
           "* TODO %?\n :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n%i" :empty-lines 1)
          ("j" "Journal" entry (file+datetree (expand-file-name "journal.org" org-directory))
           "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1))))

(use-package ob-core
  :ensure org
  :defer t
  :config
  (progn
    (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
    ;; Make babel results blocks lowercase
    (setq org-babel-results-keyword "results")
    (defun bh/display-inline-images ()
      (condition-case nil
          (org-display-inline-images)
        (error nil)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (python . t)
       (C . t)
       (octave . t)
       (emacs-lisp . t)
       (latex . t)
       (dot . t)
       (gnuplot . t)))))

(use-package ox
  :ensure org
  :defer t
  :config (load "~/.org-publish-projects.el" 'noerror))

(use-package calendar
  :defer t
  :config (setq calendar-week-start-day 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org2blog
(use-package org2blog
  :ensure t
  :defer t
  :config (load "~/.org-blogs.el" 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * info-look
(use-package info-look
  :commands info-lookup-add-help)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * latex
(use-package tex-site
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (progn
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
                 '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))))))

(use-package latex
  :ensure auctex
  :defer t
  :config
  (progn
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
                 ("(latex2e)Command Index")))))

(use-package pstricks
  :load-path "site-lisp"
  :ensure auctex
  :defer t)

(use-package reftex
  :ensure auctex
  :defer t
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
  :ensure t
  :defer t)

;; pd-mode
(use-package pd-mode
  :mode (("\\.pat$" . pd-mode)
         ("\\.pd$" . pd-mode)))

;; cmake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; desktop-entry-mode
(use-package desktop-entry-mode
  :load-path "site-lisp"
  :init (add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)
  :commands (desktop-entry-mode)
  :mode ("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))

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
;; (ptrv/add-auto-mode 'conf-unix-mode "\\.*rc$")

;; json
(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode))

;; gnuplot
(use-package gnuplot
  :defer t
  :commands (gnuplot-mode gnuplot-make-buffer)
  :mode ("\\.gp$" . gnuplot-mode))

;;;; * abbrev
(use-package abbrev
  :defer t
  :init (setq-default abbrev-mode t)
  :config
  (progn
    (define-abbrev-table 'global-abbrev-table
      '(
        ;; typo corrections
        ("teh" "the")
        ))
    (setq save-abbrevs nil))
  :diminish abbrev-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\README\\.md" . gfm-mode))
  :config
  (setq markdown-css-path (expand-file-name "css/markdown.css" ptrv/etc-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pandoc
(use-package pandoc-mode
  :ensure t
  :defer t
  :mode ("\\.text$" . markdown-mode)
  :pre-load
  (progn
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    (add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * golang

(use-package go-mode
  :ensure t
  :commands (ptrv/go-create-package)
  :defer t
  :config
  (progn
    (bind-keys :map go-mode-map
               ("M-."       . godef-jump)
               ("C-c C-i"   . go-goto-imports)
               ("C-c C-r"   . go-remove-unused-imports)
               ("C-c C-p"   . ptrv/go-create-package)
               ("C-c C-c c" . ptrv/go-run)
               ("C-c C-c r" . ptrv/go-run-buffer)
               ("C-c C-c b" . ptrv/go-build)
               ("C-c C-c t" . ptrv/go-test))

    ;; compile functions
    (defun ptrv/go-build ()
      "compile project"
      (interactive)
      (compile "go build"))

    (defun ptrv/go-test (arg)
      "test project"
      (interactive "P")
      (compile (concat "go test" (if arg " -v"))))

    (defun ptrv/read-compile--cmd (default-cmd read-cmd?)
      (if read-cmd? (compilation-read-command default-cmd) default-cmd))

    (defun ptrv/go-run (arg)
      "go run current package"
      (interactive "p")
      (let (files-list
            go-list-result
            go-list-result-list
            (rawresult (process-lines
                        "go" "list" "-f"
                        "{{range .GoFiles}}{{.}},{{end}}{{range .CgoFiles}}{{.}},{{end}}")))
        (when rawresult
          ;; get package files as list
          (setq go-list-result-list
                (s-split ","
                         (car (process-lines
                               "go" "list" "-f"
                               "{{range .GoFiles}}{{.}},{{end}}{{range .CgoFiles}}{{.}},{{end}}"))
                         t))
          ;; escape space in file names
          (when go-list-result-list)
          (setq go-list-result
                (mapcar
                 (lambda (x) (s-replace " " "\\ " x)) go-list-result-list))
          (setq files-list (s-join " " go-list-result))
          (compile (concat (ptrv/read-compile--cmd "go run" (= arg 16)) " " files-list
                           (if (= arg 4) (concat " " (read-from-minibuffer "Args: "))))))))

    (defun ptrv/go-run-buffer (arg)
      "go run current buffer"
      (interactive "p")
      (compile (concat
                (ptrv/read-compile--cmd "go run" (= arg 16)) " " buffer-file-name
                (if (= arg 4) (concat " " (read-from-minibuffer "Args: "))))))

    (defun ptrv/go-mode-init ()
      (add-hook 'before-save-hook 'gofmt-before-save nil :local)
      (whitespace-mode -1))
    (add-hook 'go-mode-hook 'ptrv/go-mode-init)

    (use-package company-go
      :ensure t
      :init (add-hook 'go-mode-hook
                      (lambda () (setq-local company-backends
                                             '((company-go :with company-yasnippet)))))
      :config (setq company-go-show-annotation nil))

    (use-package go-eldoc
      :ensure t
      :init (add-hook 'go-mode-hook #'go-eldoc-setup))

    (with-eval-after-load 'yasnippet
      (add-hook 'go-mode-hook #'yas-minor-mode))
    (with-eval-after-load 'hideshow
      (add-hook 'go-mode-hook #'hs-minor-mode))
    (with-eval-after-load 'flycheck
      (defvar flycheck-check-syntax-automatically)
      (defun ptrv/go-mode-flycheck--init ()
        (setq-local flycheck-check-syntax-automatically '(save)))
      (add-hook 'go-mode-hook 'ptrv/go-mode-flycheck--init))
    (with-eval-after-load 'whitespace-cleanup-mode
      (add-hook 'go-mode-hook (lambda () (whitespace-cleanup-mode -1))))

    (with-eval-after-load 'find-file-in-project
      (defvar ffip-patterns)
      (add-to-list 'ffip-patterns "*.go"))

    (defvar ptrv/go-default-namespaces '("github.com/ptrv" "example"))

    (defun ptrv/go-create-package (name &optional arg)
      "Create a new sketch with NAME under GOPATH src folder.

If ARG is not nil, create package in current directory"
      (interactive "sInsert new package name: \nP")
      (let ((name (remove ?\s name))
            (get-root-dir (lambda ()
                            (concat (car (split-string (getenv "GOPATH") ":"))
                                    "/src/" (completing-read
                                             "Use namespace:"
                                             ptrv/go-default-namespaces)))))
        (if (not (string-equal "" name))
            (progn
              (unless arg
                (setq name (concat (file-name-as-directory (funcall get-root-dir)) name)))
              (make-directory name)
              (find-file (concat (file-name-as-directory name)
                                 (read-from-minibuffer "File name: " "main.go"))))
          (error "Please insert a package name"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * xml
(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.gpx$" . nxml-mode))
  :defer t
  :config
  (progn
    (defun gpx-setup ()
      (when (and (stringp buffer-file-name)
                 (string-match "\\.gpx\\'" buffer-file-name))
        (setq-local nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
        (setq-local nxml-heading-element-name-regexp "name\\|time")))
    (add-hook 'nxml-mode-hook 'gpx-setup)

    (setq nxml-slash-auto-complete-flag t
          nxml-sexp-element-flag t)

    (defun xml-format ()
      "Format XML file with xmllint."
      (interactive)
      (save-excursion
        (shell-command-on-region (point-min) (point-max)
                                 "xmllint --format -" (buffer-name) t)))
    (bind-key "C-c M-h" 'xml-format nxml-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * erc
(use-package erc
  :defer t
  :config
  (progn
    (setq erc-server "irc.freenode.net"
          erc-port 6667
          erc-nick "ptrv"
          erc-nick-uniquifier "_"
          ;; erc-server-connect-function 'erc-open-tls-stream
          )

    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)

    (use-package erc-services
      :init (erc-services-mode)
      :config
      (progn
        (setq erc-prompt-for-nickserv-password nil)
        (let ((freenode-credentials (netrc-credentials "freenode"))
              (oftc-credentials (netrc-credentials "oftc")))
          (setq erc-nickserv-passwords
                `((freenode (,(cons (car freenode-credentials)
                                    (cadr freenode-credentials))))
                  (oftc (,(cons (car oftc-credentials)
                                (cadr oftc-credentials)))))))))

    (use-package erc-track
      :config
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                      "324" "329" "332" "333" "353" "477")))

    (add-hook 'erc-mode-hook
              (lambda ()
                (add-hook 'window-configuration-change-hook
                          (lambda ()
                            (setq erc-fill-column (- (window-width) 2)))
                          nil :local)))

    (setq erc-hide-list '("JOIN" "PART" "QUIT"))

    (use-package alert
      :ensure t)

    (use-package erc-match
      :init
      (add-hook 'erc-text-matched-hook
                (lambda (match-type nickuserhost message)
                  "Notify when a message is received."
                  (unless (posix-string-match "^\\** *Users on #" message)
                    (alert (replace-regexp-in-string " +" " " message)
                           :title (format "%s in %s"
                                          ;; Username of sender
                                          (car (split-string nickuserhost "!"))
                                          ;; Channel
                                          (or (erc-default-target) "#unknown")))))))))

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

(use-package subword-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'subword-mode))

(with-eval-after-load 'subword
  (diminish 'subword-mode))

(use-package delsel
  :defer t
  :init (delete-selection-mode))

(use-package bug-reference
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'bug-reference-prog-mode)
    (add-hook 'text-mode-hook 'bug-reference-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * move-text
(use-package move-text
  :ensure t
  :bind (([C-S-up]   . move-text-up)
         ([C-S-down] . move-text-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * file commands
(use-package ptrv-files
  :load-path "site-lisp"
  :bind (("<f5>"      . ptrv/refresh-file)
         ("C-c f r"   . ptrv/ido-recentf-open)
         ("C-c f o"   . ptrv/open-with)
         ("C-c f d"   . ptrv/launch-directory)
         ("C-c f R"   . ptrv/rename-current-buffer-file)
         ("C-c f D"   . ptrv/delete-file-and-buffer)
         ("C-c f w"   . ptrv/copy-file-name-to-clipboard)
         ("C-c f i"   . ptrv/find-user-init-file)
         ("C-c f b i" . ptrv/byte-recompile-init)
         ("C-c f b s" . ptrv/byte-recompile-site-lisp)
         ("C-c f b e" . ptrv/byte-recompile-elpa)
         ("C-c f b h" . ptrv/byte-recompile-home)))

(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(use-package dired
  :defer t
  :config
  (progn
    (require 'dired-x)
    (setq dired-auto-revert-buffer t
          dired-listing-switches "-alhF")

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    (dolist (file '(".ropeproject" "setup.py"))
      (add-to-list 'projectile-project-root-files file t)))
  :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ffip
(use-package find-file-in-project
  :ensure t
  :bind ("C-x f" . ffip)
  :config
  (setq ffip-project-file '(".git" ".hg" ".ropeproject" "setup.py" "project.clj")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * find-file
(use-package find-file
  :bind ("C-c o" . ff-find-other-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * processing
(use-package processing-mode
  :load-path "site-lisp/processing2-emacs"
  :defer t
  :mode ("\\.pde$" . processing-mode)
  :commands (processing-find-sketch)
  :config
  (progn
    (with-eval-after-load 'yasnippet
      (add-hook 'processing-mode-hook #'yas-minor-mode))

    (use-package processing-snippets
      :load-path "site-lisp/processing2-emacs"
      :commands (processing-snippets-initialize)
      :init (with-eval-after-load 'yasnippet
              (processing-snippets-initialize)))

    (use-package processing-company
      :load-path "site-lisp/processing2-emacs"
      :commands (processing-company-setup)
      :init (with-eval-after-load 'company
              (processing-company-setup)))

    (bind-keys :map processing-mode-map
               ("C-c C-c" . processing-sketch-run)
               ("C-c C-d" . processing-find-in-reference))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-get-checker-for-buffer
             flycheck-may-enable-mode)
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-highlighting-mode 'lines)

    (defun ptrv/flycheck-mode-on-safe ()
      (when (and (flycheck-may-enable-mode)
                 (flycheck-get-checker-for-buffer))
        (flycheck-mode)))
    (advice-add 'flycheck-mode-on-safe :override
                #'ptrv/flycheck-mode-on-safe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * hideshow
(use-package hideshow
  :defer t
  :bind (("<f12>"   . hs-toggle-hiding)
         ("S-<f12>" . hs-toggle-hiding-all))
  :init (add-hook 'prog-mode-hook #'hs-minor-mode)
  :config
  (progn
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
        (hs-show-all))))
  :diminish hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * diminish
(defmacro rename-modeline (package-name mode new-name)
  "Rename modeline for PACKAGE-NAME and MODE to NEW-NAME."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "clojure-mode" clojure-mode "λ")
(rename-modeline "python" python-mode "Py")
(rename-modeline "lisp-mode" emacs-lisp-mode "EL")
(rename-modeline "markdown-mode" markdown-mode "md")
(rename-modeline "processing-mode" processing-mode "P5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * osx
(use-package ns-win
  :if *is-mac*
  :defer t
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
  (progn
    (setq default-input-method "MacOSX")

    ;; Make cut and paste work with the OS X clipboard
    (when (not window-system)
      (setq interprogram-cut-function 'ptrv/paste-to-osx)
      (setq interprogram-paste-function 'ptrv/copy-from-osx))

    ;; Work around a bug on OS X where system-name is a fully qualified
    ;; domain name
    (setq system-name (car (split-string system-name "\\.")))

    ;; Ignore .DS_Store files with ido mode
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;;GNU ls and find
(use-package files
  :defer t
  :config
  (when *is-mac*
    (let ((gnu-ls (executable-find "gls")))
      (if gnu-ls
          (setq insert-directory-program gnu-ls)
        (message "GNU coreutils not found. Install coreutils with homebrew.")))))

(use-package grep
  :defer t
  :config
  (when *is-mac*
    (let ((gnu-find (executable-find "gfind")))
      (when gnu-find
        (setq find-program gnu-find)))
    (let ((gnu-xargs (executable-find "gxargs")))
      (when gnu-xargs
        (setq xargs-program gnu-xargs)))))

(use-package locate
  :defer t
  :config
  (when *is-mac*
    (let ((mdfind (executable-find "mdfind")))
      (when mdfind
        (setq locate-command mdfind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * linux
(when *is-linux*
  (set-frame-font "Inconsolata-12" nil t)

  (defun setup-frame-hook (frame)
    ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
    ;;(run-with-idle-timer 0.2 nil 'toggle-fullscreen)
    )
  (add-hook 'after-make-frame-functions 'setup-frame-hook)

  ;; typeriter-mode
  (use-package typewriter-mode
    :if *is-linux*
    :load-path "site-lisp"
    :commands (typewriter-mode)
    :config
    (setq typewriter-play-command
          (concat (ptrv/get-default-sound-command) " %s")
          typewriter-sound-default
          (concat ptrv/etc-dir "sounds/9744__horn__typewriter.wav")
          typewriter-sound-end
          (concat ptrv/etc-dir "sounds/eol-bell.wav")
          typewriter-sound-return
          (concat ptrv/etc-dir "sounds/carriage-return.wav"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * sclang
(use-package ptrv-sclang
  :load-path "site-lisp"
  :commands (ptrv/sclang-start)
  :mode ("\\.\\(sc\\|scd\\)$" . ptrv/sclang-mode-loader))

(use-package sclang
  :defer t
  :config
  (progn
    (require 'ptrv-sclang)
    (ptrv/sclang-mode-loader--remove)

    (setq sclang-auto-scroll-post-buffer nil
          sclang-eval-line-forward nil
          ;;sclang-help-path '("~/.local/share/SuperCollider/Help")
          sclang-library-configuration-file "~/.sclang.cfg"
          sclang-runtime-directory "~/scwork/"
          sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")

    (with-eval-after-load 'yasnippet
      (add-hook 'sclang-mode-hook #'yas-minor-mode))
    (add-hook 'sclang-mode-hook #'subword-mode)
    (bind-keys :map sclang-mode-map
               ("C-c ]"      . sclang-pop-definition-mark)
               ("s-."        . sclang-main-stop)
               ("<s-return>" . sclang-eval-region-or-line))

    (use-package company-sclang
      :load-path "site-lisp/company-sclang"
      :commands (company-sclang-setup)
      :init (with-eval-after-load 'company
              (company-sclang-setup))
      :config (unbind-key "C-M-i" sclang-mode-map))

    (use-package sclang-snippets
      :load-path "site-lisp/sclang-snippets"
      :commands (sclang-snippets-initialize)
      :init (with-eval-after-load 'yasnippet
              (sclang-snippets-initialize)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * python
(use-package python
  :defer t
  :config
  (progn
    (setq python-check-command "flake8")
    (add-hook 'python-mode-hook 'eldoc-mode)

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
      :ensure t
      :init (add-hook 'python-mode-hook 'anaconda-mode))

    (use-package company-anaconda
      :ensure t
      :init
      (with-eval-after-load 'company
        (defun ptrv/company-anaconda--init ()
          (setq-local company-backends
                      '((company-anaconda :with company-yasnippet))))
        (add-hook 'python-mode-hook 'ptrv/company-anaconda--init)))

    (use-package highlight-indentation
      :ensure t
      :init (add-hook 'python-mode-hook 'highlight-indentation-mode))

    (use-package pyenv-mode
      :ensure t
      :init (pyenv-mode)
      :config
      (progn
        (unbind-key "C-c C-s" pyenv-mode-map)
        (unbind-key "C-c C-u" pyenv-mode-map)
        (bind-keys :map pyenv-mode-map
                   ("C-. C-s" . pyenv-mode-set)
                   ("C-. C-u" . pyenv-mode-unset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ggtags
(use-package ggtags
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * cc-mode


(use-package cc-mode
  :defer t
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :init
  (progn
    (defun ptrv/cc-mode-init ()
      (c-set-style "my-cc-mode")
      (setq c-basic-offset 4
            tab-width 4
            ;;c-indent-level 4
            c-default-style "bsd"
            indent-tabs-mode nil)
      (eldoc-mode)
      (ggtags-mode)
      (setq-local eldoc-documentation-function 'ggtags-eldoc-function)
      (setq-local split-width-threshold nil))

    (ptrv/hook-into-modes #'ptrv/cc-mode-init
                          '(c-mode-hook c++-mode-hook)))
  :config
  (progn
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
      (progn
        (defvar doxymacs-external-xml-parser-executable)
        (unless (file-exists-p doxymacs-external-xml-parser-executable)
          (warn "The doxymacs_parser executable does not exist!"))
        (defvar doxymacs-mode)
        (add-hook 'font-lock-mode-hook
                  (lambda ()
                    (when (and doxymacs-mode
                               (or (eq major-mode 'c-mode)
                                   (eq major-mode 'c++-mode)))
                      (doxymacs-font-lock))))))

    (defun ptrv/c++-mode-init ()
      ;; (doxymacs-mode +1)
      (yas-minor-mode +1))
    (add-hook 'c++-mode-hook 'ptrv/c++-mode-init)

    (defun ptrv/c-mode-init()
      (local-set-key (kbd "RET") 'newline-and-indent))
    (add-hook 'c-mode-hook 'ptrv/c-mode-init)

    ;; C++11 keywords
    (font-lock-add-keywords
     'c++-mode
     `(;; complete some fundamental keywords
       ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
       ;; add the new C++11 keywords
       ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
       ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
       ;; PREPROCESSOR_CONSTANT
       ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
       ;; hexadecimal numbers
       ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
       ;; integer/float/scientific numbers
       ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
       ;; user-types (customize!)
       ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
       ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
       ) :append)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * gud
(use-package gud
  :commands gud-gdb
  :init
  (progn
    (defun ptrv/show-debugger ()
      (interactive)
      (let ((gud-buf
             (catch 'found
               (dolist (buf (buffer-list))
                 (when (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
        (if gud-buf
            (switch-to-buffer-other-window gud-buf)
          (call-interactively (if *is-mac* 'lldb 'gud-gdb)))))
    (bind-key "C-. g" #'ptrv/show-debugger))
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
  :defer t
  :commands (ycmd-mode)
  :init (add-hook 'c++-mode-hook 'ycmd-mode)
  :config
  (progn
    (use-package company-ycmd
      :load-path "~/src/emacs-ycmd"
      :commands (company-ycmd-setup)
      :init (with-eval-after-load 'company
              (company-ycmd-setup)))

    (use-package flycheck-ycmd
      :load-path "~/src/emacs-ycmd"
      :commands (flycheck-ycmd-setup)
      :init (with-eval-after-load 'flycheck
              (flycheck-ycmd-setup)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * lua
(use-package lua-mode
  :ensure t
  :defer t
  :config
  (progn
    (defun ptrv/lua-send-region-or-current-line ()
      "Send current region or line to lua process."
      (interactive)
      (if (region-active-p)
          (lua-send-region (region-beginning) (region-end))
        (lua-send-current-line)))
    (with-eval-after-load 'ycmd
      (add-hook 'lua-mode-hook 'ycmd-mode))
    (with-eval-after-load 'company-ycmd
      (defun ptrv/lua-mode-company-ycmd--init ()
        (setq-local company-backends '((company-ycmd :with company-yasnippet))))
      (add-hook 'lua-mode-hook 'ptrv/lua-mode-company-ycmd--init))
    (with-eval-after-load 'ggtags
      (add-hook 'lua-mode-hook 'ggtags-mode))
    (with-eval-after-load 'yasnippet
      (add-hook 'lua-mode-hook 'yas-minor-mode))
    (bind-keys :map lua-mode-map
               ("C-c C-d" . lua-send-proc)
               ("C-c C-c" . ptrv/lua-send-region-or-current-line)
               ("C-c C-p" . lua-start-process))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * html
(use-package sgml-mode
  :defer t
  :config
  (progn
    (require 'smartparens-html)
    (add-to-list 'sp-navigate-consider-stringlike-sexp 'html-mode)
    (bind-keys :map html-mode-map
               ("C-c C-f" . sp-html-next-tag)
               ("C-c C-b" . sp-html-previous-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * key-chord
(use-package key-chord
  :ensure t
  :defer t
  :init (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "JJ" 'ptrv/switch-to-previous-buffer)
    (key-chord-define-global "BB" 'ido-switch-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Ace jump mode
(use-package ace-jump-mode
  :ensure t
  :bind (("C-o"   . ace-jump-mode)
         ("C-S-o" . ace-jump-word-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind ("M-C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-show-preview nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * find-func
(use-package find-func
  :init (find-function-setup-keys)
  :bind (;; Help should search more than just commands
         ("C-h C-f" . find-function)
         ("C-h C-k" . find-function-on-key)
         ("C-h C-v" . find-variable)
         ("C-h C-l" . find-library)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * windows
(use-package window
  :bind (("<f6>"  . split-window-horizontally)
         ("<f7>"  . split-window-vertically)
         ("<f8>"  . delete-window)
         ("C-. z" . delete-other-windows))
  :config
  (progn
    (bind-key "C-c w ." (lambda () (interactive) (shrink-window-horizontally 4)))
    (bind-key "C-c w ," (lambda () (interactive) (enlarge-window-horizontally 4)))
    (bind-key "<down>" (lambda () (interactive) (enlarge-window -4)))
    (bind-key "<up>" (lambda () (interactive) (enlarge-window 4)))))

(use-package ptrv-window
  :load-path "site-lisp"
  :bind (("C-c w s" . ptrv/swap-windows)
         ("C-c w r" . ptrv/rotate-windows)
         ("C-c v"   . ptrv/halve-other-window-height-or-width)))

(use-package ptrv-buffers
  :load-path "site-lisp"
  :commands (ptrv/do-not-kill-important-buffers)
  :init
  (add-hook 'kill-buffer-query-functions
            #'ptrv/do-not-kill-important-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; *align
(use-package align
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; *bindings
(bind-key* "C-<return>" #'other-window)

;;fast vertical naviation
(bind-key "M-U" (lambda () (interactive) (forward-line -10)))
(bind-key "M-D" (lambda () (interactive) (forward-line 10)))

;; Align your code in a pretty way.
(bind-key "C-x \\" 'align-regexp)

;; Activate occur easily inside isearch
(bind-key "C-o" (lambda () (interactive)
                  (let ((case-fold-search isearch-case-fold-search))
                    (occur (if isearch-regexp
                               isearch-string
                             (regexp-quote isearch-string)))))
          isearch-mode-map)

(bind-key "C-m" 'newline-and-indent)

(bind-key "M-j" (lambda () (interactive) (join-line -1)))

;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
(bind-key "C-x O" (lambda () (interactive) (other-window -1)))

(bind-key "C-c T d" #'toggle-debug-on-error)
(bind-key "C-c h b" #'describe-personal-keybindings)

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
         ("C-x r v" . ptrv/list-registers)
         ("C-c u" . ptrv/browse-url)
         ("C-c q" . ptrv/exit-emacs-client)
         ("M-;" . ptrv/comment-dwim-line)
         ([remap move-beginning-of-line] . ptrv/smarter-move-beginning-of-line)
         ("C-M-\\" . ptrv/indent-region-or-buffer)
         ("C-M-z" . ptrv/indent-defun)
         ("C-c i d" . ptrv/insert-current-date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * defuns
;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun ptrv/switch-to-previous-buffer ()
  "Switch to previous open buffer.
Repeated invocation toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun ptrv/user-first-name ()
  "Get user's first name."
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))
(defun ptrv/user-first-name-p ()
  "Check whether the user name is provided."
  (not (string-equal "" (ptrv/user-first-name))))

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

(defun ptrv/set-variables-local (var-list)
  "Make all variables in VAR-LIST local."
  (unless (listp var-list)
    (error "You have to pass a list to this function"))
  (mapc (lambda (x) (make-local-variable x)) var-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * server
(use-package server
  :defer t
  :idle (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * welcome-message stuff
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
