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
(defvar ptrv/tmp-dir      (file-name-as-directory (locate-user-emacs-file "tmp")))
(defvar ptrv/autosaves-dir(file-name-as-directory (concat ptrv/tmp-dir "autosaves")))
(defvar ptrv/backups-dir  (file-name-as-directory (concat ptrv/tmp-dir "backups")))
(defvar ptrv/pscratch-dir (file-name-as-directory (concat ptrv/tmp-dir "pscratch")))

;; create tmp dirs if necessary
(make-directory ptrv/tmp-dir t)
(make-directory ptrv/autosaves-dir t)
(make-directory ptrv/backups-dir t)
(make-directory ptrv/pscratch-dir t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * backup
(setq auto-save-list-file-name
      (expand-file-name "autosave-list" ptrv/autosaves-dir))
(setq auto-save-file-name-transforms
      `((".*" ,(concat ptrv/autosaves-dir "\\1") t)))
(setq backup-directory-alist
      `((".*" . ,ptrv/backups-dir)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * PATH
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(when *is-mac*
  (defun ptrv/homebrew-prefix (&optional formula)
    "Get the homebrew prefix for FORMULA.

Without FORMULA, get the homebrew prefix itself.

Return nil, if homebrew is not available, or if the prefix
directory does not exist.
Source: `https://github.com/lunaryorn/.emacs.d'"
    (let ((prefix (condition-case nil
                      (car (apply #'process-lines "brew" "--prefix"
                                  (when formula (list formula))))
                    (error nil))))
      (when (and prefix (file-directory-p prefix))
        prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * custom settings
(defconst ptrv/custom-file (locate-user-emacs-file "custom.el"))
(use-package cus-edit
  :defer t
  :init (load ptrv/custom-file :no-error :no-message)
  :config (setq custom-file ptrv/custom-file))

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

(defun ptrv/colorize-compilation-buffer ()
    "Taken from `https://github.com/lunaryorn/.emacs.d'"
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

;; Compilation from Emacs
(use-package compile
  :defer t
  :config
  (progn
    ;; Colorize output of Compilation Mode, see
    ;; http://stackoverflow.com/a/3072831/355252
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook 'ptrv/colorize-compilation-buffer)
    ;; other settings
    (setq compilation-scroll-output t)))

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode t
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash t
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t
      font-lock-maximum-decoration t
      ;; color-theme-is-global t
      ring-bell-function 'ignore
      vc-follow-symlinks t
      diff-switches "-u"
      completion-cycle-threshold 5
      x-select-enable-clipboard t
      ;; from https://github.com/technomancy/better-defaults
      x-select-enable-primary nil
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      url-configuration-directory (expand-file-name "url" ptrv/tmp-dir))

(use-package apropos
  :defer t
  :bind (("C-c h a" . apropos)
         ("C-c h A" . apropos-command)))

(use-package autoinsert
  :init (auto-insert-mode))
(use-package jka-cmpr-hook
  :init (auto-compression-mode))
(use-package winner
  :init (winner-mode)
  :config
  (progn
    (bind-key "C-c w b" 'winner-undo)
    (bind-key "C-c w f" 'winner-redo)))

(use-package windmove
  :config (windmove-default-keybindings 'super))

(use-package recentf
  :defer t
  :init (recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "recentf" ptrv/tmp-dir)
        recentf-max-saved-items 100))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file (expand-file-name "places" ptrv/tmp-dir))))

;; savehist keeps track of some history
(use-package savehist
  :init (savehist-mode)
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" ptrv/tmp-dir)))

;; desktop.el
(use-package desktop
  :init (desktop-save-mode)
  :config (setq desktop-save 'if-exists))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing)
        whitespace-line-column nil))    ; Use `fill-column' for overlong lines

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (dolist (it '(prog-mode-hook text-mode-hook))
    (add-hook it 'whitespace-cleanup-mode))
  :config
  (setq whitespace-cleanup-mode-only-if-initially-clean t))

;; disabled commands
(setq disabled-command-function nil)

;;enable cua-mode for rectangular selections
(use-package cua-base
  :defer t
  :config (setq cua-enable-cua-keys nil))

(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" ptrv/tmp-dir)))

(defun ptrv/get-default-sound-command ()
  "Get default command for playing sound files."
  (cond
   (*is-mac* (executable-find "afplay"))
   (*is-linux* (executable-find "paplay"))))

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
    (define-key flyspell-mode-map "\M-\t" nil)
    (define-key flyspell-mode-map (kbd "C-:") 'flyspell-auto-correct-word)
    (define-key flyspell-mode-map (kbd "C-.") 'ispell-word)))

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
  :bind (("C-c f r" . ptrv/ido-recentf-open)
         ("C-x M-f" . ido-find-file-other-window))
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-max-directory-size 100000
        ido-save-directory-list-file (expand-file-name "ido.last" ptrv/tmp-dir)))

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
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-save-file (expand-file-name "smex-items" ptrv/tmp-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * idomenu
(use-package idomenu
  :ensure t
  :bind (("C-x C-i" . idomenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Eshell
(use-package eshell
  :bind (("C-x m" . eshell))
  :commands (pcomplete/go pcomplete/lein)
  :config
  (progn
    (message "Eshell config has been loaded !!!")
    (setq eshell-directory-name (locate-user-emacs-file "eshell/"))

    (bind-key "C-x M" (lambda () (interactive) (eshell t)))

    (defun eshell/clear ()
      "04Dec2001 - sailor, to clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))

    (defun eshell/e (file)
      (find-file file))

    (autoload 'pcomplete/apt-get "pcmpl-apt" nil nil)))

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

    (let ((map smartparens-strict-mode-map))
      (dolist (it sp-paredit-bindings)
        (define-key map (read-kbd-macro (car it)) (cdr it))))

    (bind-key "M-q" #'sp-indent-defun smartparens-strict-mode-map)
    (bind-key "C-j" #'sp-newline smartparens-strict-mode-map)
    (bind-key "M-?" #'sp-convolute-sexp smartparens-strict-mode-map)

    ;;"Enable `smartparens-mode' in the minibuffer, during
    ;;`eval-expression'."
    (add-hook 'eval-expression-minibuffer-setup-hook
              'smartparens-strict-mode)

    (defun ptrv/smartparens-setup-lisp-modes (modes)
      "Setup Smartparens Lisp support in MODES.

Add Lisp pairs and tags to MODES, and use the a special, more strict
keymap `ptrv/smartparens-lisp-mode-map'."
      (when (symbolp modes)
        (setq modes (list modes)))
      (sp-local-pair modes "(" nil :bind "M-(")
      (dolist (mode modes)
        (let ((hook (intern (format "%s-hook" (symbol-name mode)))))
          (add-hook hook 'smartparens-strict-mode))))))

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

    (dolist (hook '(ptrv/remove-elc-on-save fontify-headline))
      (add-hook 'emacs-lisp-mode-hook hook))

    (ptrv/smartparens-setup-lisp-modes '(emacs-lisp-mode
                                         lisp-interaction-mode
                                         lisp-mode))

    (bind-key "C-c C-z" 'switch-to-ielm emacs-lisp-mode-map)

    (bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)
    (bind-key "C-c C-e" 'eval-and-replace lisp-mode-shared-map)
    (bind-key "C-c C-p" 'eval-print-last-sexp lisp-mode-shared-map)
    (bind-key "M-RET" 'ptrv/lisp-describe-thing-at-point lisp-mode-shared-map)))

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish eldoc-mode)

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package lexbind-mode
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

(use-package ielm
  :defer t
  :config
  (ptrv/smartparens-setup-lisp-modes '(inferior-emacs-lisp-mode)))

(use-package inf-lisp
  :defer t
  :config
  (ptrv/smartparens-setup-lisp-modes '(inferior-lisp-mode)))

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
  (setq tramp-backup-directory-alist backup-directory-alist))
(use-package tramp-cache
  :defer t
  :config
  (setq tramp-persistency-file-name (expand-file-name "tramp" ptrv/tmp-dir)))

(defun sudo-edit (&optional arg)
  "Edit buffer with superuser privileges."
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
;; newline after 72 chars in magit-log-edit-mode
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
    (bind-keys :map magit-status-mode-map
               ("q" . magit-quit-session)
               ("Q" . (lambda () (interactive) (magit-quit-session t))))

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

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

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
  :init (global-diff-hl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * mercurial
(use-package ahg
  :ensure t)

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

    (unless yas-global-mode (yas-reload-all))))

(use-package dropdown-list
  :ensure yasnippet
  :defer t
  :config (add-to-list 'yas-prompt-functions 'yas-dropdown-prompt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * undo-tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * insert-time.el
(use-package insert-time
  :load-path "site-lisp/insert-time"
  :config
  (setq insert-date-format "%Y-%m-%d"
        insert-time-format "%H:%M:%S"))

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
  (progn
    (bind-key "C-c C-p p" 'sql-set-product sql-mode-map)
    (bind-key "C-c C-p i" 'sql-set-sqli-buffer sql-mode-map)))

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
;;;; * xah lee modes
(use-package xmsi-math-symbols-input
  :load-path "site-lisp/misc"
  :defer t
  :commands (xmsi-mode)
  :config
  (progn
    (bind-key "S-SPC" 'nil xmsi-keymap)
    ;; (bind-key "C-c C-8" 'xmsi-change-to-symbol xmsi-keymap)
    ))

;; xub-mode
(use-package xub-mode
  :load-path "site-lisp/misc"
  :defer t
  :commands (xub-mode))
(defalias 'unicode-browser 'xub-mode)

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
  :bind (("C-<next>" . iflipb-next-buffer)
         ("C-<prior>" . iflipb-previous-buffer)
         ("<XF86Forward>" . iflipb-next-buffer)
         ("<XF86Back>" . iflipb-previous-buffer))
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
  :init
  (progn
    (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
    (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
    (edit-server-start))
  :config
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))
        edit-server-new-frame nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * iedit
(use-package iedit
  :ensure t)

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
            ("*compilation*" :noselect t)
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

;; Do not allow to kill the *scratch* buffer
(defvar unkillable-scratch-buffer-erase)
(setq unkillable-scratch-buffer-erase nil)
(defun toggle-unkillable-scratch-buffer-erase ()
  (interactive)
  (if unkillable-scratch-buffer-erase
      (progn
        (setq unkillable-scratch-buffer-erase nil)
        (message "Disable scratch-buffer erase on kill!"))
    (setq unkillable-scratch-buffer-erase t)
    (message "Enable scratch-buffer erase on kill!")))

(defun unkillable-scratch-buffer ()
  "Make scratch buffer unkillable."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (if unkillable-scratch-buffer-erase
            (delete-region (point-min) (point-max)))
        nil
        )
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; make file executabable on save if has shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * org
(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c A" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (progn
    (message "Org config has been loaded !!!")

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
  :load-path "site-lisp"
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
    (message "TeX config has been loaded !!!")
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
    (message "LaTeX config has been loaded !!!")

    (dolist (hook '(LaTeX-math-mode reftex-mode auto-fill-mode))
      (add-hook 'LaTeX-mode-hook hook))

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
  :load-path "site-lisp/misc"
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
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" .  yaml-mode)
         ("\\.ya?ml$" .  yaml-mode)))

;; pd-mode
(use-package pd-mode
  :mode (("\\.pat$" . pd-mode)
         ("\\.pd$" . pd-mode)))

;; gitconfig
(use-package gitconfig-mode
  :ensure t
  :mode ("gitconfig*" . gitconfig-mode))

;; cmake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; desktop-entry-mode
(use-package desktop-entry-mode
  :load-path "site-lisp/misc"
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
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.mkd$" . markdown-mode))
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
  :bind (("M-." . godef-jump)
         ("C-c i" . go-goto-imports)
         ("C-c C-r" . go-remove-unused-imports)
         ("C-c C-p" . ptrv/go-create-package)
         ("C-c C-c c" . ptrv/go-run)
         ("C-c C-c r" . ptrv/go-run-buffer)
         ("C-c C-c b" . ptrv/go-build)
         ("C-c C-c t" . ptrv/go-test))
  :defer t
  :config
  (progn
    (message "go-mode config has been loaded !!!")

    ;; (defun ptrv/locate-godoc-src-file (f)
    ;;   (concat (car (split-string (getenv "GOPATH") ":")) "/src/" f))

    ;; (add-to-list 'load-path (ptrv/locate-godoc-src-file
    ;;                          "github.com/nsf/gocode/emacs-company"))

    ;; compile fucntions
    (defun ptrv/go-build ()
      "compile project"
      (interactive)
      (compile "go build"))

    (defun ptrv/go-test (arg)
      "test project"
      (interactive "P")
      (compile (concat "go test" (if arg " -v"))))

    ;; (defun ptrv/go-chk ()
    ;;   "gocheck project"
    ;;   (interactive)
    ;;   (compile "go test -gocheck.vv"))

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
      (yas-minor-mode +1)
      (add-hook 'before-save-hook 'gofmt-before-save nil :local)
      (hs-minor-mode +1)
      (go-eldoc-setup)
      (setq-local flycheck-check-syntax-automatically '(save))
      (whitespace-mode -1)
      (whitespace-cleanup-mode -1))

    (add-hook 'go-mode-hook 'ptrv/go-mode-init)

    (with-eval-after-load 'find-file-in-project
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

(use-package company-go
  :ensure go-mode
  :defer t
  :init (add-hook 'go-mode-hook
                  (lambda () (setq-local company-backends
                                         '((company-go :with company-yasnippet)))))
  :config (setq company-go-show-annotation nil))

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
          nxml-sexp-element-flag t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * erc
;; (defun erc-connect ()
;;   "Start up erc and connect to freedonde."
;;   (interactive)
;;   (require 'erc)
;;   (erc :server "irc.freenode.net"
;;        :full-name "Peter V."
;;        :port 6667
;;        :nick "ptrv"))

;; (ptrv/after erc
;;   (ptrv/after erc-services
;;     (setq erc-prompt-for-nickserv-password nil)
;;     (let ((freenode-credentials (netrc-credentials "freenode"))
;;           (oftc-credentials (netrc-credentials "oftc")))
;;       (setq erc-nickserv-passwords
;;             `((freenode (,(cons (car freenode-credentials)
;;                                 (cadr freenode-credentials))))
;;               (oftc (,(cons (car oftc-credentials)
;;                             (cadr oftc-credentials))))))))
;;   (erc-services-mode +1)

;;   ;;IRC
;;   (ptrv/after erc-join
;;     (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))

;;     (cond ((string= system-name "alderaan")
;;            (setq erc-autojoin-channels-alist
;;                  (list (append (car erc-autojoin-channels-alist)
;;                                '("#supercollider" "#archlinux")))))
;;           ((string= system-name "anoth")
;;            (setq erc-autojoin-channels-alist
;;                  (list (append (car erc-autojoin-channels-alist)
;;                                '("#supercollider" "#archlinux")))))
;;           ;; (t (setq erc-autojoin-channels-alist
;;           ;;          '(("freenode.net" "#emacs" "#clojure" "overtone"))))
;;           ))
;;   (erc-autojoin-mode +1)

;;   (ptrv/after erc-match
;;     (setq erc-keywords '("ptrv")))
;;   (erc-match-mode +1))

;; (ptrv/after erc-track
;;   (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;;                                   "324" "329" "332" "333" "353" "477")))

;; (make-variable-buffer-local 'erc-fill-column)
;; (ptrv/after erc
;;   ;;change wrap width when window is resized
;;   (add-hook 'window-configuration-change-hook
;;             (lambda ()
;;               (save-excursion
;;                 (walk-windows
;;                  (lambda (w)
;;                    (let ((buffer (window-buffer w)))
;;                      (set-buffer buffer)
;;                      (when (eq major-mode 'erc-mode)
;;                        (setq erc-fill-column (- (window-width w) 2)))))))))

;;   (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * faust-mode
(use-package faust-mode
  :load-path "site-lisp/emacs-faust-mode"
  :mode ("\\.dsp$" . faust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Synth-A-Modeler mode
(use-package sam-mode
  :load-path "site-lisp/misc"
  :mode ("\\.mdl$" . sam-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * editing
(setq sentence-end-double-space nil)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(bind-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;;(global-subword-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

(delete-selection-mode)

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * move-text
(use-package move-text
  :ensure t
  :bind (([C-S-up] . move-text-up)
         ([C-S-down] . move-text-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * file commands
(defun ptrv/get-standard-open-command ()
  "Get the standard command to open a file."
  (cond
   (*is-mac* "open")
   (*is-linux* "xdg-open")))

;; http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun ptrv/open-with (arg)
  "Open the file visited by the current buffer externally.

Use the standard program to open the file.  With prefix ARG,
prompt for the command to use."
  (interactive "P")
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (let ((marked-files (dired-get-marked-files)))
                          (if marked-files
                              marked-files
                            (directory-file-name (dired-current-directory))))
                      (list (buffer-file-name))))
         (doIt (if (<= (length file-list) 5)
                   t
                 (y-or-n-p "Open more than 5 files?"))))
    (when doIt
      (unless (car file-list)
        (user-error "This buffer is not visiting a file"))
      (let ((command (unless arg (ptrv/get-standard-open-command))))
        (unless command
          (setq command (read-shell-command "Open current file with: ")))
        (let ((open-fn (lambda (file-path)
                         (cond
                          (*is-linux*
                           (let ((process-connection-type nil))
                             (start-process "" nil command (shell-quote-argument file-path))))
                          (*is-mac*
                           (shell-command
                            (concat command " " (shell-quote-argument file-path))))))))
          (mapc open-fn file-list))))))

(defun ptrv/launch-directory ()
  "Open parent directory in external file manager."
  (interactive)
  (let ((command (ptrv/get-standard-open-command))
        (dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
                (expand-file-name default-directory))))
    (cond
     (*is-linux*
      (let ((process-connection-type nil))
        (start-process "" nil command dir)))
     (*is-mac*
      (shell-command (concat command " " dir))))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun ptrv/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun ptrv/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ptrv/delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (when (y-or-n-p "Delete file and its buffer?")
    (let ((filename (buffer-file-name)))
      (cond
       ((not filename) (kill-buffer))
       ((vc-backend filename) (vc-delete-file filename))
       (:else
        (delete-file filename)
        (kill-buffer))))))

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun ptrv/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(bind-key "C-c f o" 'ptrv/open-with)
(bind-key "C-c f d" 'ptrv/launch-directory)
(bind-key "C-c f r" 'ptrv/ido-recentf-open)
(bind-key "C-c f R" 'ptrv/rename-current-buffer-file)
(bind-key "C-c f D" 'ptrv/delete-file-and-buffer)
(bind-key "C-c f w" 'ptrv/copy-file-name-to-clipboard)
(bind-key "C-c f i" 'ptrv/find-user-init-file)
(bind-key "C-c f b i" 'ptrv/byte-recompile-init)
(bind-key "C-c f b s" 'ptrv/byte-recompile-site-lisp)
(bind-key "C-c f b e" 'ptrv/byte-recompile-elpa)
(bind-key "C-c f b h" 'ptrv/byte-recompile-home)

(use-package dired
  :defer t
  :config
  (progn
    (require 'dired-x)
    (setq dired-auto-revert-buffer t
          dired-listing-switches "-ahl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-known-projects-file (expand-file-name
                                          "projectile-bookmarks.eld"
                                          ptrv/tmp-dir)
          projectile-cache-file (expand-file-name "projectile.cache" ptrv/tmp-dir))

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
;;;; * processing
(use-package processing-mode
  :load-path "site-lisp/processing2-emacs"
  :mode ("\\.pde$" . processing-mode)
  :commands (processing-snippets-initialize processing-find-sketch)
  :defer t
  :config
  (progn
    (with-eval-after-load 'yasnippet
      (processing-snippets-initialize))

    (cond (*is-mac*
           (setq processing-location "/usr/bin/processing-java")
           (setq processing-application-dir "/Applications/Processing.app")
           (setq processing-sketchbook-dir "~/Documents/Processing"))
          (*is-linux*
           (setq processing-location "~/applications/processing/processing-java")
           (setq processing-application-dir "~/applications/processing")
           (setq processing-sketchbook-dir "~/sketchbook")))

    (defvar ptrv/processing-keywords
      (cons 'processing-mode (append processing-functions
                                     processing-builtins
                                     processing-constants)))

    (defun ptrv/processing-company--init ()
      (setq-local company-backends '((company-keywords :with company-yasnippet)))
      (make-local-variable 'company-keywords-alist)
      (add-to-list 'company-keywords-alist ptrv/processing-keywords))
    (add-hook 'processing-mode-hook 'ptrv/processing-company--init)

    (defun ptrv/processing-mode-init ()
      (yas-minor-mode +1))
    (add-hook 'processing-mode-hook 'ptrv/processing-mode-init)

    (let ((map processing-mode-map))
      (define-key map (kbd "C-c C-c") 'processing-sketch-run)
      (define-key map (kbd "C-c C-d") 'processing-find-in-reference))

    ;; If htmlize is installed, provide this function to copy buffer or
    ;; region to clipboard
    (when (and (fboundp 'htmlize-buffer)
               (fboundp 'htmlize-region))
      (defun processing-copy-as-html (&optional arg)
        ""
        (interactive "P")
        (if (eq (buffer-local-value 'major-mode (get-buffer (current-buffer)))
                'processing-mode)
            (save-excursion
              (let ((htmlbuf (if (region-active-p)
                                 (htmlize-region (region-beginning) (region-end))
                               (htmlize-buffer))))
                (if arg
                    (switch-to-buffer htmlbuf)
                  (with-current-buffer htmlbuf
                    (clipboard-kill-ring-save (point-min) (point-max)))
                  (kill-buffer htmlbuf)
                  (message "Copied as HTML to clipboard"))))
          (message (concat "Copy as HTML failed, because current "
                           "buffer is not a Processing buffer."))))
      (define-key processing-mode-map (kbd "C-c C-p z") 'processing-copy-as-html)
      (easy-menu-add-item processing-mode-menu nil (list "---"))
      (easy-menu-add-item processing-mode-menu nil
                          ["Copy as HTML" processing-copy-as-html
                           :help "Copy buffer or region as HTML to clipboard"]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-highlighting-mode 'lines)
    (dolist (it '(;; emacs-lisp-mode-hook
                  lisp-interaction-mode-hook))
      (add-hook it (lambda () (flycheck-mode -1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * hideshow
(use-package hideshow
  :defer t
  :bind (("<f11>" . hs-toggle-hiding)
         ("S-<f11>" . hs-toggle-hiding-all))
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-mode-hook c-mode-common-hook
                    perl-mode-hook sh-mode-hook
                    python-mode-hook))
      (add-hook hook 'hs-minor-mode))

    (bind-keys* :map hs-minor-mode-map
                ("C-c @ h" . hs-hide-block)
                ("C-c @ H" . hs-show-block)
                ("C-c @ s" . hs-hide-all)
                ("C-c @ S" . hs-show-all)
                ("C-c @ l" . hs-hide-level)
                ("C-c @ c" . hs-toggle-hiding)
                ([(shift mouse-2)] . hs-mouse-toggle-hiding))

    ;; https://github.com/Hawstein/my-emacs/blob/master/_emacs/hs-minor-mode-settings.el
    (setq hs-isearch-open t)

    (defvar hs-hide-all nil
      "Current state of hideshow for toggling all.")
    (defun ptrv/hideshow-init ()
      (ptrv/set-variables-local '(hs-hide-all))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'hs-minor-mode-map ptrv/hs-minor-mode-map)))
    (add-hook 'hs-minor-mode-hook 'ptrv/hideshow-init)

    (defun hs-toggle-hiding-all ()
      "Toggle hideshow all."
      (interactive)
      (setq hs-hide-all (not hs-hide-all))
      (if hs-hide-all
          (hs-hide-all)
        (hs-show-all)))

    (defadvice goto-line
        (after
         expand-after-goto-line
         activate compile)
      "hideshow-expand affected block when using goto-line in a
collapsed buffer"
      (save-excursion
        (hs-show-block)))))

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
(when *is-mac*
  (use-package ns-win
    :defer t
    :config
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'super
          mac-function-modifier 'hyper
          ;; mac-right-command-modifier 'super
          mac-right-option-modifier nil
          ))

  (when *is-cocoa-emacs*
    (set-frame-font "Inconsolata-16" nil t)
    (set-frame-size (selected-frame) 110 53)
    (set-frame-position (selected-frame) 520 24))

  (setq default-input-method "MacOSX")

  ;; Make cut and paste work with the OS X clipboard

  (defun ptrv/copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun ptrv/paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (when (not window-system)
    (setq interprogram-cut-function 'ptrv/paste-to-osx)
    (setq interprogram-paste-function 'ptrv/copy-from-osx))

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")

  ;;GNU ls and find
  (use-package files
    :defer t
    :config
    (let ((gls (executable-find "gls")))
      (if gls
          (setq insert-directory-program gls)
        (message "GNU coreutils not found. Install coreutils with homebrew."))))

  (use-package grep
    :defer t
    :config
    (let ((gfind (executable-find "gfind")))
      (when gfind
        (setq find-program gfind))))

  (use-package locate
    :defer t
    :config
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

  ;; erc notification
  ;; (ptrv/after erc
  ;;   (defun my-notify-erc (match-type nickuserhost message)
  ;;     "Notify when a message is received."
  ;;     (unless (posix-string-match "^\\** *Users on #" message)
  ;;       (alert (replace-regexp-in-string " +" " " message)
  ;;        :title (format "%s in %s"
  ;;                       ;; Username of sender
  ;;                       (car (split-string nickuserhost "!"))
  ;;                       ;; Channel
  ;;                       (or (erc-default-target) "#unknown")))))

  ;;   (add-hook 'erc-text-matched-hook 'my-notify-erc))

  ;; typeriter-mode
  (use-package typewriter-mode
    :defer t
    :config
    (setq typewriter-play-command (concat
                                   (ptrv/get-default-sound-command)
                                   " %s")
          typewriter-sound-default (concat
                                    ptrv/etc-dir
                                    "sounds/9744__horn__typewriter.wav")
          typewriter-sound-end (concat
                                ptrv/etc-dir
                                "sounds/eol-bell.wav")
          typewriter-sound-return (concat
                                   ptrv/etc-dir
                                   "sounds/carriage-return.wav"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * sclang
;; (defun ptrv/sclang-mode-loader ()
;;   "Load sclang-mode."
;;   (unless (featurep 'sclang)
;;     (require 'sclang)
;;     (sclang-mode)
;;     (ptrv/sclang-mode-loader--remove)))
;; (defun ptrv/sclang-mode-loader--remove ()
;;   "Remove `ptrv/sclang-mode-loader' from `auto-mode-alist'."
;;   (delete (rassq 'ptrv/sclang-mode-loader auto-mode-alist)
;;           auto-mode-alist))
;; (ptrv/add-auto-mode 'ptrv/sclang-mode-loader "\\.\\(sc\\|scd\\)$")

;; (defun ptrv/sclang ()
;;   "Start sclang-mode."
;;   (interactive)
;;   (if (require 'sclang nil t)
;;       (progn
;;         (sclang-start)
;;         (ptrv/sclang-mode-loader--remove))
;;     (message "SCLang is not installed!")))

;; ;; (use-package sclang)
;; (use-package sclang-mode
;;   :config
;;   (progn
;;     (message "sclang config has been loaded !!!")
;;     (setq sclang-auto-scroll-post-buffer nil
;;           sclang-eval-line-forward nil
;;           ;;sclang-help-path '("~/.local/share/SuperCollider/Help")
;;           sclang-library-configuration-file "~/.sclang.cfg"
;;           sclang-runtime-directory "~/scwork/"
;;           sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")

;;     (with-eval-after-load 'company
;;       (add-to-list 'load-path (locate-user-emacs-file "site-lisp/company-sclang"))
;;       (require 'company-sclang)
;;       (defun ptrv/sclang-company--init()
;;         (setq-local company-backends '((company-sclang
;;                                         company-yasnippet
;;                                         company-dabbrev-code)))
;;         (make-local-variable 'company-dabbrev-code-modes)
;;         (add-to-list 'company-dabbrev-code-modes 'sclang-mode))
;;       (add-to-list 'sclang-mode-hook 'ptrv/sclang-company--init))

;;     (defun ptrv/sclang-init ()
;;       (yas-minor-mode +1)
;;       (setq indent-tabs-mode nil)
;;       (subword-mode +1))
;;     (add-hook 'sclang-mode-hook 'ptrv/sclang-init)
;;     ;; (add-hook 'sclang-mode-hook 'sclang-extensions-mode)

;;     (defun ptrv/sclang-all-windows-to-front ()
;;       "Raise all supercollider windows."
;;       (interactive)
;;       (sclang-eval-string "Window.allWindows.do(_.front);"))

;;     (defun ptrv/sclang-ido-switch-to-buffer ()
;;       (interactive)
;;       (let* ((blist (buffer-list))
;;              (predicate (lambda (b)
;;                           (with-current-buffer b
;;                             (derived-mode-p 'sclang-mode))))
;;              (sc-buffers (delq nil (mapcar
;;                                     (lambda (b)
;;                                       (if (funcall predicate b) b nil))
;;                                     blist))))
;;         (pop-to-buffer-same-window
;;          (ido-completing-read "SCLang buffers: "
;;                               (mapcar 'list (mapcar 'buffer-name sc-buffers))))))

;;     (with-eval-after-load 'sclang-mode
;;       (bind-keys :map sclang-mode-map
;;                  ("C-c ]" . sclang-pop-definition-mark)
;;                  ("C-c F" . ptrv/sclang-all-windows-to-front)
;;                  ("s-." . sclang-main-stop)
;;                  ("<s-return>" . sclang-eval-region-or-line)
;;                  ("C-c C-b" . ptrv/sclang-ido-switch-to-buffer)))

;;     (defun ptrv/sclang--show-window (win-cmd-str transparent? &optional alpha)
;;       ""
;;       (let* ((alpha-val (or alpha 0.6))
;;              (transparency-code (if transparent?
;;                                     (concat
;;                                      ".window.alpha = "
;;                                      (number-to-string alpha-val)))))
;;         (sclang-eval-string (concat win-cmd-str transparency-code ";"))))

;;     (defun ptrv/sclang-show-meter (arg)
;;       "Show level meter."
;;       (interactive "P")
;;       (ptrv/sclang--show-window "Server.default.meter" arg))
;;     (defun ptrv/sclang-show-scope (arg)
;;       "Show scope."
;;       (interactive "P")
;;       (ptrv/sclang--show-window "Server.default.scope(numChannels: 2)" arg))
;;     (defun ptrv/sclang-show-helper-window (arg)
;;       "Show helper window."
;;       (interactive "P")
;;       (ptrv/sclang--show-window "HelperWindow.new" arg))
;;     (defun ptrv/sclang-show-node-tree ()
;;       "Show tree window."
;;       (interactive)
;;       (sclang-eval-string "Server.default.plotTree;"))

;;     (with-eval-after-load 'sclang-server
;;       (bind-keys :map sclang-server-key-map
;;                  ("l" . ptrv/sclang-show-meter)
;;                  ("s" . ptrv/sclang-show-scope)
;;                  ("h" . ptrv/sclang-show-helper-window)
;;                  ("t" . ptrv/sclang-show-node-tree)))

;;     ;; snippets
;;     (autoload 'sclang-snippets-initialize "sclang-snippets" nil nil)
;;     (ptrv/after yasnippet
;;       (sclang-snippets-initialize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * python
(use-package python
  :defer t
  :config
  (progn
    (setq python-check-command "flake8")
    (add-hook 'python-mode-hook 'eldoc-mode)

    ;; info
    (info-lookup-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     '(("(python)Index" nil "")))))

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode)
  :init (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :config
  (progn
    (defun ptrv/company-anaconda--init ()
      (setq-local company-backends
                  '((company-anaconda :with company-yasnippet))))
    (add-hook 'python-mode-hook 'ptrv/company-anaconda--init)))

(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode)
  :init (add-hook 'python-mode-hook 'highlight-indentation-mode))

(use-package pyenv-mode
  :ensure t
  :commands (pyenv-mode)
  :init (add-hook 'python-mode-hook 'pyenv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * cc-mode
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(use-package cc-mode
  :defer t
  :config
  (progn
    (message "Load config: cc-mode...")

    (c-add-style "my-cc-mode"
                 '("bsd"
                   (c-basic-offset . 4)
                   (c-offsets-alist . ((innamespace . 0)))))

    (defun ptrv/cc-mode-init ()
      (c-set-style "my-cc-mode")
      (setq c-basic-offset 4
            tab-width 4
            ;;c-indent-level 4
            c-default-style "bsd"
            indent-tabs-mode nil)
      (local-set-key (kbd "C-c o") 'ff-find-other-file)
      (eldoc-mode +1)
      (ggtags-mode +1)
      (setq-local eldoc-documentation-function 'ggtags-eldoc-function)
      (local-set-key (kbd "C-c C-c") 'compile)
      (setq-local split-width-threshold nil))

    (dolist (it '(c-mode-hook c++-mode-hook))
      (add-hook it 'ptrv/cc-mode-init))

    ;; doxymacs
    (defvar ptrv/doxymacs-path
      (cond
       (*is-mac*
        (concat (ptrv/homebrew-prefix "doxymacs")
                "/share/emacs/site-lisp"))
       (*is-linux*
        (locate-user-emacs-file
         "site-lisp/_extras/doxymacs/lisp"))
       (t nil)))

    (when ptrv/doxymacs-path
      (add-to-list 'load-path ptrv/doxymacs-path)
      (autoload 'doxymacs-mode "doxymacs" nil t))
    (with-eval-after-load 'doxymacs
      (when *is-linux*
        (setq doxymacs-external-xml-parser-executable
              (locate-user-emacs-file
               "site-lisp/_extras/doxymacs/c/doxymacs_parser"))
        (unless (file-exists-p doxymacs-external-xml-parser-executable)
          (warn "The doxymacs_parser executable does not exist!")))

      (defun ptrv/doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'ptrv/doxymacs-font-lock-hook))

    (defun ptrv/c++-mode-init ()
      ;; (doxymacs-mode +1)
      ;; (flycheck-mode -1)
      (yas-minor-mode +1))
    (add-hook 'c++-mode-hook 'ptrv/c++-mode-init)

    (defun ptrv/c-mode-init()
      (local-set-key (kbd "RET") 'newline-and-indent)
      (local-set-key (kbd "C-c C-c") 'compile))
    (add-hook 'c-mode-hook 'ptrv/c-mode-init)

    ;; (add-hook 'c-mode-common-hook 'linum-mode)

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

(use-package gud-lldb
  :if *is-mac*
  :commands (lldb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * irony
;; (ptrv/after cc-mode
;;   (message "Load config: Irony...")

;;   (defun ptrv/company-irony--init ()
;;     (setq-local company-backends '(company-irony company-clang))
;;     (local-set-key (kbd "<f12>") 'company-complete-common)
;;     (irony-cdb-autosetup-compile-options))
;;   (ptrv/add-to-hook 'c++-mode-hook '(irony-mode ptrv/company-irony--init))
;;   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;   (ptrv/after flycheck
;;     (add-to-list 'flycheck-checkers 'irony)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * ycmd
(use-package ycmd
  :load-path "~/src/emacs-ycmd"
  :defer t
  :init (add-hook 'c++-mode-hook 'ycmd-mode))

(use-package company-ycmd
  :load-path "~/src/emacs-ycmd"
  :init (company-ycmd-setup))

(use-package flycheck-ycmd
  :load-path "~/src/emacs-ycmd"
  :init (flycheck-ycmd-setup))

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
    (defun ptrv/lua-mode-init ()
      (ggtags-mode +1)
      (yas-minor-mode +1)
      (local-set-key (kbd "C-c C-d") 'lua-send-proc)
      (local-set-key (kbd "C-c C-c") 'ptrv/lua-send-region-or-current-line)
      (local-set-key (kbd "C-c C-p") 'lua-start-process)
      (ycmd-mode +1)
      (setq-local company-backends '((company-ycmd :with company-yasnippet))))
    (add-hook 'lua-mode-hook 'ptrv/lua-mode-init)))

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
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (with-eval-after-load 'multiple-cursors-core
    (setq mc/list-file (expand-file-name "mc-lists.el" ptrv/tmp-dir))))

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
  (progn
    (key-chord-define-global "JJ" 'switch-to-previous-buffer)
    (key-chord-define-global "BB" 'ido-switch-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * Ace jump mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-o" . ace-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind ("M-C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-show-preview nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * bindings
(use-package find-func
  :init (find-function-setup-keys)
  :bind (;; Help should search more than just commands
         ("C-h C-f" . find-function)
         ("C-h C-k" . find-function-on-key)
         ("C-h C-v" . find-variable)
         ("C-h C-l" . find-library)))

(bind-key "<f5>" 'refresh-file)

;; Split Windows
(use-package window
  :bind (("<f6>" . split-window-horizontally)
         ("<f7>" . split-window-vertically)
         ("<f8>" . delete-window)
         ("<f9>" . delete-other-windows))
  :config
  (progn
    (bind-key "C-c w ." (lambda () (interactive) (shrink-window-horizontally 4)))
    (bind-key "C-c w ," (lambda () (interactive) (enlarge-window-horizontally 4)))
    (bind-key "<down>" (lambda () (interactive) (enlarge-window -4)))
    (bind-key "<up>" (lambda () (interactive) (enlarge-window 4)))))

;;diff shortcuts
(bind-key "C-c D d" 'diff)
(bind-key "C-c D f" 'diff-buffer-with-file)

(bind-key "C-c w s" 'swap-windows)
(bind-key "C-c w r" 'rotate-windows)

;;fast vertical naviation
(bind-key "M-U" (lambda () (interactive) (forward-line -10)))
(bind-key "M-D" (lambda () (interactive) (forward-line 10)))

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "M-C-%" 'query-replace)

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

(bind-key "M-;" 'comment-dwim-line)

(bind-key "<S-return>" 'ptrv/smart-open-line)
(bind-key "M-o" 'ptrv/smart-open-line)
(bind-key "<C-S-return>" 'ptrv/smart-open-line-above)

(bind-key [remap goto-line] 'goto-line-with-feedback)

(bind-key "M-j" (lambda () (interactive) (join-line -1)))

(bind-key "C-M-\\" 'indent-region-or-buffer)
(bind-key "C-M-z" 'indent-defun)

;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
(bind-key "C-x O" (lambda () (interactive) (other-window -1)))

;; registers
(bind-key "C-x r v" 'ptrv/list-registers)

(bind-key "C-M-=" 'increment-number-at-point)
(bind-key "C-M--" 'decrement-number-at-point)

(bind-key "C-c d" 'ptrv/duplicate-current-line-or-region)
(bind-key "C-c M-d"
          'ptrv/duplicate-and-comment-current-line-or-region)

(bind-key "C-c q" 'exit-emacs-client)
(bind-key "C-c t" 'ptrv/eshell-or-restore)
(bind-key "C-c u" 'ptrv/browse-url)
(bind-key "C-c v" 'halve-other-window-height-or-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * defuns
(defun ptrv/ido-recentf-open ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun refresh-file ()
  "Revert file in current buffer."
  (interactive)
  (revert-buffer nil t)
  (message "Buffer reverted!"))

(defun ptrv/display-yank-menu ()
  "Open function `yank-menu' popup."
  (interactive)
  (popup-menu 'yank-menu))

;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.

Pass ARG to `comment-dwim'.  If no region is selected and current
line is not blank and we are not at the end of the line, then
comment current line.  Replaces default behaviour of `comment-dwim,'
when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; http://irreal.org/blog/?p=1742
(defun ptrv/eshell-or-restore ()
  "Bring up a full-screen eshell or restore previous config."
  (interactive)
  (if (string= "eshell-mode" major-mode)
      (jump-to-register :eshell-fullscreen)
    (window-configuration-to-register :eshell-fullscreen)
    (eshell)
    (delete-other-windows)))

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;; https://github.com/banister/window-rotate-for-emacs
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  "Rotate windows."
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun ptrv/byte-recompile-site-lisp ()
  "Recompile user site-lisp directory."
  (interactive)
  (dolist (project (directory-files
                    (locate-user-emacs-file "site-lisp") t "^[^_]\\w+"))
    (when (file-directory-p project)
      (byte-recompile-directory project 0))))

(defun ptrv/byte-recompile-elpa ()
  "Recompile elpa directory."
  (interactive)
  (when (boundp 'package-user-dir)
    (byte-recompile-directory package-user-dir 0)))

(defun ptrv/byte-recompile-init ()
  "Recompile user's init file."
  (interactive)
  (byte-recompile-file (locate-user-emacs-file "init.el") t 0))

(defun ptrv/byte-recompile-home ()
  "Recompile all relevant files in user's Emacs dir."
  (interactive)
  (ptrv/byte-recompile-site-lisp)
  (ptrv/byte-recompile-elpa)
  (ptrv/byte-recompile-init))

;; Recreate scratch buffer
(defun create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (when (zerop (buffer-size))
    (insert initial-scratch-message)
    (set-buffer-modified-p nil)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun ptrv/duplicate-current-line-or-region (arg &optional do-comment)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated.  If DO-COMMENT is non-nil, comment current line or
region."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end))
          (end-orig end) end-comment)
      (when do-comment
        (comment-or-uncomment-region beg end)
        (setq end (line-end-position))
        (setq end-comment end))
      (let ((it 0))
        (while (< it arg)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point))
          (setq it (1+ it))))
      (goto-char (+ origin (* (length region) arg) arg
                    ;; when commenting, advance current point with
                    ;; number of comment characters times marked lines
                    ;; to maintain cursor position
                    (if do-comment
                        (- end-comment end-orig) 0))))))

(defun ptrv/duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated."
  (interactive "p")
  (ptrv/duplicate-current-line-or-region arg t))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun exit-emacs-client ()
  "Consistent exit emacsclient.

If not in emacs client, echo a message in minibuffer, don't exit
emacs.  If in server mode and editing file, do C-x # server-edit
else do C-x 5 0 delete-frame"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (delete-frame)))

(defun ptrv/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun ptrv/smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (ptrv/smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
number input."
  (interactive)
  (let ((line-numbers-off-p (if (boundp 'linum-mode)
                                (not linum-mode)
                              t)))
    (unwind-protect
        (progn
          (when line-numbers-off-p
            (linum-mode 1))
          (call-interactively 'goto-line))
      (when line-numbers-off-p
        (linum-mode -1))))
  (save-excursion
    (hs-show-block)))

(defun toggle-window-split ()
  "Toggle window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; http://stackoverflow.com/a/4988206
(defun halve-other-window-height-or-width (arg)
  "Expand current window to use half of the other window's lines."
  (interactive "P")
  (let ((win-dim-fn (if arg 'window-width 'window-height))
        (win-division-factor (if arg 4 2)))
    (enlarge-window (/ (funcall win-dim-fn (next-window)) win-division-factor) arg)))

(defun xml-format ()
  "Format XML file with xmllint."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)))

;; http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (indent-buffer)
      (message "Indented buffer."))))

;; http://emacsredux.com/blog/2013/03/28/indent-defun/
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun ptrv/user-first-name ()
  "Get user's first name."
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))
(defun ptrv/user-first-name-p ()
  "Check whether the user name is provided."
  (not (string-equal "" (ptrv/user-first-name))))

;; http://emacsredux.com/blog/2013/03/30/kill-other-buffers/
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;; Don't sort the registers list because it might contain keywords
(defun ptrv/list-registers ()
  "Display a list of nonempty registers saying briefly what they contain."
  (interactive)
  (let ((list (copy-sequence register-alist)))
    ;;(setq list (sort list (lambda (a b) (< (car a) (car b)))))
    (with-output-to-temp-buffer "*Registers*"
      (dolist (elt list)
        (when (get-register (car elt))
          (describe-register-1 (car elt))
          (terpri))))))

;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-previous-buffer ()
  "Switch to previous open buffer.
Repeated invocation toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun switch-to-ielm ()
  "Switch to an ielm window.
Create a new ielm process if required."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

;;http://www.emacswiki.org/emacs/IncrementNumber
(defun ptrv/change-number-at-point (change)
  "Change number at point with CHANGE fn."
  (save-excursion
    (save-match-data
      (or (looking-at "[0123456789]")
          (error "No number at point"))
      (replace-match
       (number-to-string
        (mod (funcall change (string-to-number (match-string 0))) 10))))))
(defun increment-number-at-point ()
  "Increment number at point."
  (interactive)
  (ptrv/change-number-at-point '1+))
(defun decrement-number-at-point ()
  "Decrement number at point."
  (interactive)
  (ptrv/change-number-at-point '1-))

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

(defun ptrv/browse-url ()
  "Open rlf in default browser."
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (if url (browse-url url) (message "No URL at point!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; * server
(use-package server
  :init (unless (server-running-p) (server-start)))

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
