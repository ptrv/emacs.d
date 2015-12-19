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
  (declare (indent 1))
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(bind-key "C-c h b" #'describe-personal-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * tls
(defcustom ptrv/trustfile-command nil
  "Command to retrieve trustfile path.

Something like: `python -m certifi'."
  :type 'string)

;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package tls
  :defer t
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
  :config (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * custom settings
(defconst ptrv/custom-file (locate-user-emacs-file "custom.el"))
(use-package cus-edit
  :defer t
  :init (load ptrv/custom-file :no-error :no-message)
  :config (setq custom-file ptrv/custom-file))

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
  :bind (("C-x p" . pop-to-mark-command)
         ("C-m" . newline-and-indent)
         ("M-j" . ptrv/join-line))
  :init
  (defun ptrv/join-line ()
    (interactive)
    (join-line -1))
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
  :config (auto-insert-mode))
(use-package jka-cmpr-hook
  :config (auto-compression-mode))
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
  :config (if (fboundp 'save-place-mode)
             (save-place-mode 1)
           (setq-default save-place t)))

(setq history-length 1000)
(use-package savehist
  :config
  (progn
    (savehist-mode t)
    (setq savehist-additional-variables '(search ring regexp-search-ring)
         savehist-autosave-interval 60
         savehist-save-minibuffer-history t)))

;; desktop.el
(use-package desktop
  :config
  (progn
    (desktop-save-mode)
    (setq desktop-save 'if-exists)))

(use-package whitespace
  :bind ("C-c T w" . whitespace-mode)
  :init (ptrv/hook-into-modes #'whitespace-mode
          '(prog-mode-hook text-mode-hook))
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
  :bind (("C-c T W" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))
  :init (ptrv/hook-into-modes #'whitespace-cleanup-mode
          '(prog-mode-hook text-mode-hook))
  :config
  (progn
    (setq whitespace-cleanup-mode-only-if-initially-clean t)
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'go-mode)))

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

(use-package "mule-cmds"
  :defer t
  :config
  (bind-key "C-x <return>" mule-keymap))

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
      (when (eq major-mode 'compilation-mode)
        (ansi-color-process-output nil)
        (setq-local comint-last-output-start (point-marker))))
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
;;;; * timdate
(use-package time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Bucharest"  "Satu Mare")
                                  ("America/New_York" "New York (USA)")
                                  ("America/San_Francisco" "San Francisco")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))

(use-package calendar
  :bind ("C-c u c" . calendar)
  :config (setq calendar-week-start-day 1))

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
  :config (global-hl-line-mode))

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
;;;; * beacon
(use-package beacon
  :ensure t
  :init (add-hook 'after-init-hook #'beacon-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ido
(use-package ido
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1))
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
  :config
  (progn
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
                   `(disable exact ,(symbol-name cmd))))))

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
         ("C-c u s" . ptrv/eshell-or-restore))
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
      :load-path "site-lisp")

    (use-package pcmpl-git
      :ensure t)

    (use-package starter-kit-eshell
      :ensure t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * company
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook #'global-company-mode)
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
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)))

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

    (sp-local-pair sp--lisp-modes "(" nil :bind "M-(")

    (dolist (mode sp--lisp-modes)
      (unless (eq mode 'eshell-mode)
        (let ((hook (intern (format "%s-hook" (symbol-name mode)))))
          (add-hook hook 'smartparens-strict-mode))))

    (ptrv/hook-into-modes #'smartparens-strict-mode
      '(eval-expression-minibuffer-setup-hook ielm-mode-hook))

    (sp-local-pair '(c++-mode) "{" nil :post-handlers
                   '(((lambda (&rest _ignored)
                        (ptrv/smart-open-line-above)) "RET")))))

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
        (bind-key "C-c z" 'ptrv/switch-to-ielm)))

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
      (bind-key "C-M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map))))

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :init
  (ptrv/hook-into-modes #'elisp-slime-nav-mode
    '(emacs-lisp-mode-hook ielm-mode-hook))
  :diminish elisp-slime-nav-mode)

(use-package eldoc
  :defer t
  :init (ptrv/hook-into-modes #'eldoc-mode
          '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook
            ielm-mode-hook
            eval-expression-minibuffer-setup-hook
            c-mode-common-hook
            python-mode-hook
            cider-mode-hook
            cider-repl-mode-hook))
  :diminish eldoc-mode)

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (ptrv/hook-into-modes #'rainbow-delimiters-mode
    '(text-mode-hook prog-mode-hook)))

(use-package macrostep
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'lisp-mode
    (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
    (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(bind-key "C-c T d" #'toggle-debug-on-error)

(use-package eval-sexp-fu
  :ensure t
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * clojure
(use-package cider
  :ensure t
  :defer t
  :config
  (progn
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

    (with-eval-after-load 'smartparens
      (sp-local-pair 'clojure-mode "(" nil :bind "M-("))

    (use-package cider-eval-sexp-fu
      :ensure t
      :demand t)

    ;; (use-package clj-refactor
    ;;   :ensure t)
    ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ffap
(use-package ffap
  :defer t
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ibuffer
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :init (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  :config
  (progn
    (use-package ibuf-ext
      :config (setq ibuffer-show-empty-filter-groups nil))))

(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'ibuffer
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
                     ("root" (filename . "^/sudo:root.*"))
                     ("Org" (mode . org-mode)))
                   (ibuffer-projectile-generate-filter-groups)))
            (unless (eq ibuffer-sorting-mode 'filename/process)
              (ibuffer-do-sort-by-filename/process)))
          (add-hook 'ibuffer-hook
                    #'ptrv/ibuffer-group-buffers)))

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
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v b" . magit-blame))
  :config
  (progn
    (setq magit-completing-read-function
          #'magit-ido-completing-read)

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
              #'ptrv/magit-set-repo-dirs-from-projectile)))

(use-package git-commit
  :ensure t
  :defer t
  :config
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :bind ("C-c v t" . git-timemachine-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * vc
(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package vc-git
  :defer t
  :config
  (when (version-list-< (version-to-list emacs-version) '(25 0 50 0))
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
        (message "There are unresolved conflicts in this file")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * git-messenger
(use-package git-messenger
  :ensure t
  :defer t
  :bind (("C-c v p" . git-messenger:popup-message))
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
  :init
  (ptrv/hook-into-modes #'yas-minor-mode
    '(lua-mode-hook
      c++-mode-hook
      sclang-mode-hook
      processing-mode-hook
      go-mode-hook
      clojure-mode-hook))
  :config
  (progn
    (setq yas-prompt-functions '(yas-x-prompt
                                 yas-ido-prompt
                                 yas-completing-prompt))

    (unless yas-global-mode (yas-reload-all))

    (use-package dropdown-list
      :ensure t
      :init
      (add-to-list 'yas-prompt-functions 'yas-dropdown-prompt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * undo-tree
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
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
;;;; * frames
;; display visited file's path as frame title
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;; defalias
(defalias 'toggle-fullscreen 'toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * fullframe
(use-package fullframe
  :ensure t
  :defer t
  :init (with-eval-after-load 'magit
          (fullframe magit-status magit-mode-quit-window)))

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
(use-package "isearch"                    ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point))
  :config
  (bind-key "C-o" (lambda () (interactive)
                    (let ((case-fold-search isearch-case-fold-search))
                      (occur (if isearch-regexp
                                 isearch-string
                               (regexp-quote isearch-string)))))
            isearch-mode-map))

(use-package highlight-symbol
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

;; the silver searcher
(use-package ag
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'highlight-symbol
    (defun highlight-symbol-ag ()
      (interactive)
      (if (thing-at-point 'symbol)
          (let ((highlight-symbol-border-pattern '("\\b" . "\\b")))
            (ag-project-regexp (highlight-symbol-get-symbol)))
        (error "No symbol at point")))
    (bind-key "C-c s a" #'highlight-symbol-ag))
  :bind(("C-c a a" . ag-regexp)
        ("C-c a A" . ag)
        ("C-c a d" . ag-dired-regexp)
        ("C-c a D" . ag-dired)
        ("C-c a f" . ag-files)
        ("C-c a k" . ag-kill-other-buffers)
        ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package anzu
  :ensure t
  :defer t
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
  :defer t
  :init
  (progn
    (edit-server-start)
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
  :bind ("C-;" . iedit-mode))

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
  :config
  (progn
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
            ("*wclock*" :noselect t :height ,(ptrv/get-popwin-height 'small))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * buffer
(use-package autorevert
  :config
  (progn
    (global-auto-revert-mode)
    ;; Also auto refresh dired, but be quiet about it
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)))

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
  :bind (("C-c O a" . org-agenda)
         ("C-c O b" . org-iswitchb)
         ("C-c O c" . org-capture)
         ("C-c O o" . org-store-link))
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
        org-mobile-files '("~/org/ptrv.org"
                           "~/org/notes.org"
                           "~/org/journal.org")
        org-mobile-inbox-for-pull "~/org/from-mobile.org"))

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
  :disabled t
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
;; (ptrv/add-auto-mode 'conf-unix-mode "\\.*rc$")

;; json
(use-package json-reformat
  :ensure t
  :defer t
  :bind ("C-c e j" . json-reformat-region))

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
  (setq markdown-css-paths
        (list
         (expand-file-name "css/pandoc.css" ptrv/etc-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * pandoc
(use-package pandoc-mode
  :ensure t
  :defer t
  :mode ("\\.text$" . markdown-mode)
  :init
  (progn
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    (add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jira
(use-package jira-markup-mode
  :ensure t
  :mode ("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * golang
(use-package go-mode
  :ensure t
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

    (defun ptrv/go-mode-init ()
      (add-hook 'before-save-hook 'gofmt-before-save nil :local))
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
      (add-hook 'go-mode-hook 'ptrv/go-mode-flycheck--init))))

(use-package ptrv-go
  :load-path "site-lisp"
  :commands (ptrv/go-create-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * xml
(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.gpx$" . nxml-mode))
  :bind ("C-c e x" . xml-format)
  :init
  (progn
    (defun xml-format ()
      "Format XML file with xmllint."
      (interactive)
      (if (executable-find "xmllint")
          (when (eq major-mode 'nxml-mode)
            (save-excursion
              (shell-command-on-region
               (point-min) (point-max) "xmllint --format -" (buffer-name) t)))
        (user-error "The executable `xmllint' not found!"))))
  :config
  (progn
    (defun gpx-setup ()
      (when (and (stringp buffer-file-name)
                 (string-match "\\.gpx\\'" buffer-file-name))
        (setq-local nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
        (setq-local nxml-heading-element-name-regexp "name\\|time")))
    (add-hook 'nxml-mode-hook 'gpx-setup)

    (setq nxml-slash-auto-complete-flag t
          nxml-sexp-element-flag t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * erc
(use-package erc
  :defer t
  :init
  (defun erc-connect ()
    (interactive)
    (let ((freenode-user (car (netrc-credentials "freenode"))))
      (if freenode-user
          (erc :server "irc.freenode.net"
               :port 7000
               :nick freenode-user)
        (user-error "Freenode user-name not found"))))
  :config
  (progn
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
      (bind-key "C-c C-b" #'ptrv/erc-switch-to-buffer erc-mode-map))))

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

;;fast vertical naviation
(bind-key "M-U" (lambda () (interactive) (forward-line -10)))
(bind-key "M-D" (lambda () (interactive) (forward-line 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * bug-reference
(use-package bug-reference
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'bug-reference-prog-mode)
    (add-hook 'text-mode-hook 'bug-reference-mode)
    (with-eval-after-load 'magit
      (ptrv/hook-into-modes #'bug-reference-mode
        '(magit-status-mode-hook magit-log-mode-hook))
      (ptrv/hook-into-modes #'hack-dir-local-variables-non-file-buffer
        '(magit-status-mode-hook magit-log-mode-hook)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * move-text
(use-package move-text
  :load-path "site-lisp"
  :bind (("C-S-<up>"   . move-text-up)
         ("C-S-<down>" . move-text-down)))

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
  :config
  (progn
    (projectile-global-mode)
    (dolist (file '(".ropeproject" "setup.py"))
      (add-to-list 'projectile-project-root-files file t))
    (run-with-idle-timer 10 nil (lambda () (projectile-cleanup-known-projects))))
  :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * find-file-in-project
(use-package find-file-in-project
  :ensure t
  :bind ("C-x f" . find-file-in-project))

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
  :bind ("C-c l e" . list-flycheck-errors)
  :commands (flycheck-get-checker-for-buffer
             flycheck-may-enable-mode)
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
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
  :init
  (progn
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
    (add-hook 'find-file-hook #'ptrv/pyenv-mode-set-maybe))
  :config
  (progn
    (setq python-check-command "flake8")

    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))

    (let ((ipython (executable-find "ipython")))
      (if ipython
          (setq python-shell-interpreter "ipython")
        (warn "IPython is missing, falling back to default python")))

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
      :config
      (progn
        (pyenv-mode)
        (unbind-key "C-c C-s" pyenv-mode-map)
        (unbind-key "C-c C-u" pyenv-mode-map)
        (bind-keys :map pyenv-mode-map
                   ("C-. p s" . pyenv-mode-set)
                   ("C-. p u" . pyenv-mode-unset))))

    (use-package pytest
      :ensure t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ggtags
(use-package ggtags
  :ensure t
  :defer t
  :init
  (ptrv/hook-into-modes #'ggtags-mode
    '(c++-mode-hook lua-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * cc-mode
(use-package cc-mode
  :defer t
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode)
         ("\\.inl\\'"                 . c++-mode))
  :init
  (progn
    (defun ptrv/cc-mode-init ()
      (c-set-style "my-cc-mode")
      (setq c-basic-offset 4
            tab-width 4
            ;;c-indent-level 4
            c-default-style "bsd"
            indent-tabs-mode nil)
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
            (switch-to-buffer gud-buf)
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
  :init
  (ptrv/hook-into-modes #'ycmd-mode
    '(c-mode-hook c++-mode-hook go-mode-hook))
  :config
  (progn
    (defun ptrv/ycmd-show-server-buffer ()
      (interactive)
      (-when-let (buf (get-buffer ycmd--server-buffer-name))
        (push-mark)
        (ring-insert find-tag-marker-ring (point-marker))
        (switch-to-buffer buf)))
    (bind-key "w" #'ptrv/ycmd-show-server-buffer ycmd-command-map)
    (use-package company-ycmd
      :load-path "~/src/emacs-ycmd"
      :commands (company-ycmd-setup)
      :init (with-eval-after-load 'company
              (company-ycmd-setup)))

    (use-package flycheck-ycmd
      :load-path "~/src/emacs-ycmd"
      :commands (flycheck-ycmd-setup)
      :init (with-eval-after-load 'flycheck
              (flycheck-ycmd-setup)))

    (defun ptrv/company-ycmd-complete ()
      (interactive)
      (let ((ycmd-force-semantic-completion t))
        (company-complete)))
    (bind-key [remap complete-symbol]
              #'ptrv/company-ycmd-complete ycmd-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * lua
(use-package lua-mode
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (defun ptrv/lua-mode-company-init ()
      (setq-local company-backends '((company-lua
                                      company-gtags
                                      company-dabbrev-code
                                      company-etags
                                      company-yasnippet))))
    (add-hook 'lua-mode-hook #'ptrv/lua-mode-company-init))
  :config
  (progn
    (defun ptrv/lua-send-region-or-current-line ()
      "Send current region or line to lua process."
      (interactive)
      (if (region-active-p)
          (lua-send-region (region-beginning) (region-end))
        (lua-send-current-line)))
    (bind-keys :map lua-mode-map
               ("C-c C-d" . lua-send-proc)
               ("C-c C-c" . ptrv/lua-send-region-or-current-line)
               ("C-c C-p" . lua-start-process))))

(use-package company-lua
  :load-path "site-lisp/company-lua"
  :defer t)

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
  :bind (("M-3"       . mc/mark-next-like-this)
         ("M-4"       . mc/mark-previous-like-this)
         ("C-M-3"     . mc/unmark-next-like-this)
         ("C-M-4"     . mc/unmark-previous-like-this)
         ("C-x C-m"   . mc/mark-all-dwim)
         ("C-c m i"   . mc/insert-numbers)
         ("C-c m h"   . mc-hide-unmatched-lines-mode)
         ("C-c m a"   . mc/mark-all-like-this)
         ("C-c m d"   . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m r"   . mc/reverse-regions)
         ("C-c m s"   . mc/sort-regions)
         ("C-c m l"   . mc/edit-lines)
         ("C-c m C-a" . mc/edit-beginnings-of-lines)
         ("C-c m C-e" . mc/edit-ends-of-lines)))

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
    (key-chord-define-global "BB" 'ido-switch-buffer)
    (key-chord-define-global "JJ" (lambda ()
                                    (interactive)
                                    (switch-to-buffer
                                     (other-buffer (current-buffer) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Avy
(use-package avy
  :ensure t
  :bind ("C-c j" . avy-goto-word-or-subword-1)
  :config (setq avy-background t
                avy-style 'at))

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
    (bind-key "C-c w <down>" (lambda () (interactive) (enlarge-window -4)))
    (bind-key "C-c w <up>" (lambda () (interactive) (enlarge-window 4)))
    ;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
    (bind-key "C-x O" (lambda () (interactive) (other-window -1)))))

(use-package ptrv-window
  :load-path "site-lisp"
  :bind (("C-c w s" . ptrv/swap-windows)
         ("C-c w r" . ptrv/rotate-windows)
         ("C-c w v" . ptrv/halve-other-window-height-or-width)))

(use-package ptrv-buffers
  :load-path "site-lisp"
  :commands (ptrv/do-not-kill-important-buffers)
  :init
  (add-hook 'kill-buffer-query-functions
            #'ptrv/do-not-kill-important-buffers))

(use-package ace-window
  :ensure t
  :bind ("C-<return>" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; *align
(use-package align
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

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
         ("C-c u b" . ptrv/browse-url)
         ("C-c q" . ptrv/exit-emacs-client)
         ("M-;" . ptrv/comment-dwim-line)
         ([remap move-beginning-of-line] . ptrv/smarter-move-beginning-of-line)
         ("C-M-\\" . ptrv/indent-region-or-buffer)
         ("C-M-z" . ptrv/indent-defun)
         ("C-c e d" . ptrv/insert-current-date)
         ("C-c e S" . sudo-edit)))

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
