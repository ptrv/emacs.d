;;; init.el --- ptrv init file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; basic init stuff
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

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist
    (project (directory-files (locate-user-emacs-file "site-lisp") t "\\w+"))
  (when (and (file-directory-p project)
             (not (string-match "_extras" project)))
    (add-to-list 'load-path project)))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (locate-user-emacs-file "loaddefs.el"))
(setq custom-file (locate-user-emacs-file "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; macros
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

Forward compatibility wrapper."
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro ptrv/after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  ;; Byte compile the body.  If the feature is not available, ignore warnings.
  ;; Taken from
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2012-11/msg01262.html
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "ptrv/after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro ptrv/with-library (feature &rest body)
  "Evaluate BODY only if require SYMBOL is successful."
  (declare (indent defun))
  `(when (require ',feature nil t)
     ,@body))

(defconst ptrv/font-lock-keywords
  `((,(concat "(" (regexp-opt '("ptrv/after"
                                "ptrv/with-library"
                                "ptrv/add-auto-mode")
                              'symbols)
              "\\>[ \t']*\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
    (,(concat "(" (regexp-opt '("with-eval-after-load")
                              'symbols))
     (1 font-lock-keyword-face))))

(ptrv/after lisp-mode
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (font-lock-add-keywords mode ptrv/font-lock-keywords :append)))
(ptrv/after ielm
  (font-lock-add-keywords 'inferior-emacs-lisp-mode
                          ptrv/font-lock-keywords :append))

(defun ptrv/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given
file `PATTERNS'."
  (declare (indent defun))
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; backup
(setq auto-save-list-file-name
      (concat ptrv/autosaves-dir "autosave-list"))
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
;;;; package
(require 'package)
(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("org" . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ptrv/packages
  '(dash
    s
    ;;vcs
    magit
    ahg
    git-commit-mode
    gitignore-mode
    gitconfig-mode
    gist
    git-messenger
    ;; editor
    iflipb
    iedit
    starter-kit-eshell
    multiple-cursors
    expand-region
    move-text
    undo-tree
    flycheck
    flymake-cursor
    autopair
    ace-jump-mode
    key-chord
    ;; ido
    smex
    ido-ubiquitous
    idomenu
    ;; completion
    auto-complete
    ac-nrepl
    pcmpl-git
    ;; snippets
    dropdown-list
    yasnippet
    ;; lisp
    paredit
    rainbow-delimiters
    mic-paren
    nrepl-eval-sexp-fu
    elisp-slime-nav
    clojure-mode
    align-cljlet
    nrepl
    litable
    lexbind-mode
    ;; markup
    org-plus-contrib
    markdown-mode
    pandoc-mode
    ;; ui
    notify
    color-theme
    popwin
    smooth-scrolling
    ;; utilities
    xml-rpc
    refheap
    pomodoro
    edit-server
    edit-server-htmlize
    tea-time
    google-this
    htmlize
    mwe-log-commands
    nyan-mode
    diminish
    kill-ring-search
    auto-compile
    ;; tools
    ag
    ack-and-a-half
    exec-path-from-shell
    w3m
    scratch
    ;; python
    pytest
    pylint
    jedi
    elpy
    virtualenv
    ;; major modes
    go-mode
    glsl-mode
    apache-mode
    yaml-mode
    cmake-mode
    lua-mode
    json-mode
    gnuplot
    pkgbuild-mode
    ;; project
    find-file-in-project
    projectile
    ;; minor-modes
    love-minor-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p ptrv/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; from carton.el and modified
(defun ptrv/package-upgrade ()
  "Upgrade packages."
  (interactive)
  (with-temp-buffer
    (package-refresh-contents)
    (package-initialize)
    (package-menu--generate nil t) ;; WTF ELPA, really???
    (let ((upgrades (package-menu--find-upgrades)))
      (dolist (upgrade upgrades)
        (let ((name (car upgrade)))
          (package-install name)))
      ;; Delete obsolete packages
      (dolist (pkg package-obsolete-alist)
        (package-delete (symbol-name (car pkg))
                        (package-version-join (caadr pkg))))
      (message "%d package%s has been upgraded."
               (length upgrades)
               (if (= (length upgrades) 1) "" "s")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PATH
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; builtins
(setq-default fill-column 72
              indent-tabs-mode nil ; And force use of spaces
              c-basic-offset 4     ; indents 4 chars
              tab-width 4)         ; and 4 char wide for TAB

(defun ptrv/get-default-url-program ()
  "Get default program to handle urls."
  (cond
   (*is-mac* (executable-find "open"))
   (*is-linux* (executable-find "x-www-browser"))))

(setq browse-url-generic-program (ptrv/get-default-url-program)
      browse-url-browser-function 'browse-url-generic
      initial-major-mode 'lisp-interaction-mode
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
      color-theme-is-global t
      ring-bell-function 'ignore
      compilation-scroll-output t
      vc-follow-symlinks t
      diff-switches "-u"
      completion-cycle-threshold 5
      x-select-enable-clipboard t
      ;; from https://github.com/technomancy/better-defaults
      x-select-enable-primary nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      url-configuration-directory (concat ptrv/tmp-dir "url"))

(auto-insert-mode 1)
(auto-compression-mode t)
(show-paren-mode 1)
(winner-mode 1)
(windmove-default-keybindings 'super)

(ptrv/after recentf
  (setq recentf-save-file (concat ptrv/tmp-dir "recentf")
        recentf-max-saved-items 100))
(recentf-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat ptrv/tmp-dir "places"))

;; savehist keeps track of some history
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat ptrv/tmp-dir "savehist"))
(savehist-mode t)

;; desktop.el
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defun ptrv/cleanup-whitespace ()
  (let ((whitespace-style '(trailing empty)) )
    (whitespace-cleanup)))
(add-hook 'before-save-hook 'ptrv/cleanup-whitespace)

;; disabled commands
(setq disabled-command-function nil)

;;enable cua-mode for rectangular selections
;; (cua-mode 1)
(setq cua-enable-cua-keys nil)
;; autopair-newline interferes with cua-rotate-rectangle (default binding "\r")
(ptrv/after cua-base
  (define-key cua--rectangle-keymap (kbd "M-<return>") 'cua-rotate-rectangle))

(setq bookmark-default-file (concat ptrv/tmp-dir "bookmarks"))

(defun ptrv/get-default-sound-command ()
  "Get default command for playing sound files."
  (cond
   (*is-mac* (executable-find "afplay"))
   (*is-linux* (executable-find "paplay"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ediff
(ptrv/after ediff
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; spelling
(ptrv/after ispell
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ;; ispell-extra-args '("--sug-mode=ultra")
        ispell-dictionary "en"          ; default dictionary
        ispell-silently-savep t))       ; Don't ask when saving the private dict

(ptrv/after flyspell
  (setq flyspell-use-meta-tab nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (define-key flyspell-mode-map "\M-\t" nil)
  (define-key flyspell-mode-map (kbd "C-:") 'flyspell-auto-correct-word)
  (define-key flyspell-mode-map (kbd "C-.") 'ispell-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hippie-expand
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
        try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; look-and-feel
(setq column-number-mode t)
(global-hl-line-mode 1)

(defvar ptrv/themes-dir (locate-user-emacs-file "themes"))
(add-to-list 'load-path ptrv/themes-dir)
(autoload 'color-theme-gandalf-ptrv "gandalf-ptrv" nil nil)
(color-theme-gandalf-ptrv)

(ptrv/with-library live-fontify-hex
  (dolist (mode '(lisp-mode emacs-lisp-mode lisp-interaction-mode css-mode))
    (font-lock-add-keywords mode '((live-fontify-hex-colors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nyan-mode
(setq nyan-bar-length 16)
(nyan-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ido
(ptrv/after ido
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-max-directory-size 100000
        ido-save-directory-list-file (concat ptrv/tmp-dir "ido.last")))
(ido-mode t)
(icomplete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ido-ubiquitous
(ptrv/after ido-ubiquitous
  (dolist (func '(tmm-prompt erc-iswitchb))
    (ido-ubiquitous-disable-in func))

  (dolist (cmd '(sh-set-shell
                 ispell-change-dictionary
                 add-dir-local-variable
                 ahg-do-command
                 sclang-dump-interface
                 sclang-dump-full-interface
                 kill-ring-search
                 tmm-menubar))
    (add-to-list 'ido-ubiquitous-command-exceptions cmd))

  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))

  ;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
  ;; (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
  (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet))
(ido-ubiquitous-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smex
(global-set-key (kbd "M-x") 'smex)
(autoload 'smex-major-mode-commands "smex" nil t)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(ptrv/after smex
  (setq smex-save-file (concat ptrv/tmp-dir "smex-items")))
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; idomenu
(global-set-key (kbd "C-x C-i") 'idomenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eshell
(ptrv/after eshell
  (message "Eshell config has been loaded !!!")
  (setq eshell-directory-name (locate-user-emacs-file "eshell/"))

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

  (ptrv/after auto-complete
    (require 'eshell-ac-pcomplete)))

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; complete
(ptrv/with-library auto-complete-config
  (ac-config-default)
  (ac-flyspell-workaround)

  (setq ac-comphist-file (concat ptrv/tmp-dir "ac-comphist.dat")
        ac-auto-show-menu t
        ac-dwim t
        ac-use-menu-map t
        ac-quick-help-delay 0.8
        ac-quick-help-height 60
        ac-disable-inline t
        ac-show-menu-immediately-on-auto-complete t
        ac-ignore-case nil
        ac-candidate-menu-min 0
        ac-auto-start nil)

  (set-default 'ac-sources
               '(ac-source-dictionary
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-semantic
                 ac-source-yasnippet))

  (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                      sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                      html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                      lisp-mode textile-mode markdown-mode tuareg-mode))
    (add-to-list 'ac-modes mode))

  (let ((map ac-completing-map))
    (define-key map (kbd "C-M-n") 'ac-next)
    (define-key map (kbd "C-M-p") 'ac-previous)
    (define-key map "\t" 'ac-complete)
    (define-key map (kbd "M-RET") 'ac-help)
    ;;(define-key map "\r" 'nil)
    (define-key map "\r" 'ac-complete))

  (ac-set-trigger-key "TAB")

  ;; complete on dot
  (defun ptrv/ac-dot-complete ()
    "Insert dot and complete code at point."
    (interactive)
    (insert ".")
    (unless (ac-cursor-on-diable-face-p)
      (auto-complete))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lisp
(autoload 'enable-paredit-mode "paredit" nil t)
(defvar ptrv/lisp-common-modes
  '(enable-paredit-mode
    rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(ptrv/add-auto-mode 'emacs-lisp-mode "\\.el$")
(defvar ptrv/emacs-lisp-common-modes
  (append
   '(turn-on-elisp-slime-nav-mode
     turn-on-eldoc-mode)
   ptrv/lisp-common-modes)
  "Common modes for Emacs Lisp editing.")
(ptrv/after lisp-mode
  (dolist (mode ptrv/emacs-lisp-common-modes)
    (add-hook 'emacs-lisp-mode-hook mode)
    (add-hook 'lisp-interaction-mode-hook mode))
  (define-key lisp-mode-shared-map (kbd "RET")
    'reindent-then-newline-and-indent)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-ielm)
  (define-key lisp-mode-shared-map (kbd "C-c C-e") 'eval-and-replace)

  (ptrv/after elisp-slime-nav
    (define-key lisp-mode-shared-map (kbd "M-RET")
      'elisp-slime-nav-describe-elisp-thing-at-point))

  (dolist (mode '(lexbind-mode auto-compile-mode))
    (add-hook 'emacs-lisp-mode-hook mode)))

(ptrv/after ielm
  (dolist (mode ptrv/emacs-lisp-common-modes)
    (add-hook 'ielm-mode-hook mode)))

(paren-activate)

(require 'highlight)
(require 'nrepl-eval-sexp-fu)
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clojure
(ptrv/after find-file-in-project
  (add-to-list 'ffip-patterns "*.clj"))

(ptrv/after clojure-mode
  (message "clojure config has been loaded !!!")

  (dolist (mode ptrv/lisp-common-modes)
    (add-hook 'clojure-mode-hook mode))

  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "ƒ")
                              nil)))))
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil)))))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq buffer-save-without-query t)))

  ;;Treat hyphens as a word character when transposing words
  (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
    (let ((st (make-syntax-table clojure-mode-syntax-table)))
      (modify-syntax-entry ?- "w" st)
      st))
  (defun live-transpose-words-with-hyphens (arg)
    "Treat hyphens as a word character when transposing words"
    (interactive "*p")
    (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
      (transpose-words arg)))

  (define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)

  (autoload 'kibit-mode "kibit-mode" nil t)
  (add-hook 'clojure-mode-hook 'kibit-mode)

  (ptrv/after kibit-mode
    (define-key kibit-mode-keymap (kbd "C-c C-n") 'nil)
    (define-key kibit-mode-keymap (kbd "C-c k c") 'kibit-check))

  (add-hook 'clojure-mode-hook (lambda () (flycheck-mode -1)))

  ;; push-mark when switching to nrepl via C-c C-z
  (defadvice nrepl-switch-to-repl-buffer (around
                                          nrepl-switch-to-repl-buffer-with-mark
                                          activate)
    (with-current-buffer (current-buffer)
      (push-mark)
      ad-do-it)))

(ptrv/add-auto-mode 'clojure-mode "\\.cljs$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 4clojure
(autoload '4clojure-problem "four-clj" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nrepl
(add-to-list 'same-window-buffer-names "*nrepl*")

(ptrv/after nrepl
  (setq nrepl-popup-stacktraces nil
        nrepl-popup-stacktraces-in-repl nil
        nrepl-port "4555")

  (add-hook 'nrepl-interaction-mode-hook
            (lambda ()
              (nrepl-turn-on-eldoc-mode)
              (enable-paredit-mode)))

  (add-hook 'nrepl-mode-hook
            (lambda ()
              (nrepl-turn-on-eldoc-mode)
              (enable-paredit-mode)
              (define-key nrepl-mode-map
                (kbd "{") 'paredit-open-curly)
              (define-key nrepl-mode-map
                (kbd "}") 'paredit-close-curly)))

  ;; Show documentation/information with M-RET
  (define-key nrepl-mode-map (kbd "M-RET") 'nrepl-doc)
  (define-key nrepl-interaction-mode-map (kbd "M-RET") 'nrepl-doc)

  ;;Auto Complete
  (dolist (hook '(nrepl-mode-hook nrepl-interaction-mode-hook))
    (add-hook hook 'ac-nrepl-setup))

  (ptrv/after auto-complete
    (add-to-list 'ac-modes 'nrepl-mode))

  ;; specify the print length to be 100 to stop infinite sequences killing things.
  (defun live-nrepl-set-print-length ()
    (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))
  (add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

  (when *is-windows*
    (defun live-windows-hide-eol ()
      "Do not show ^M in files containing mixed UNIX and DOS line endings."
      (interactive)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))
    (add-hook 'nrepl-mode-hook 'live-windows-hide-eol)

    ;; Windows M-. navigation fix
    (defun nrepl-jump-to-def (var)
      "Jump to the definition of the var at point."
      (let ((form (format "((clojure.core/juxt
                         (comp (fn [s] (if (clojure.core/re-find #\"[Ww]indows\" (System/getProperty \"os.name\"))
                                           (.replace s \"file:/\" \"file:\")
                                           s))
                               clojure.core/str
                               clojure.java.io/resource :file)
                         (comp clojure.core/str clojure.java.io/file :file) :line)
                        (clojure.core/meta (clojure.core/ns-resolve '%s '%s)))"
                          (nrepl-current-ns) var)))
        (nrepl-send-string form
                           (nrepl-jump-to-def-handler (current-buffer))
                           (nrepl-current-ns)
                           (nrepl-current-tooling-session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tramp
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

(ptrv/after tramp
  (setq tramp-backup-directory-alist backup-directory-alist
        tramp-persistency-file-name (concat ptrv/tmp-dir "tramp")))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ibuffer
(setq ibuffer-saved-filter-groups
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
         ("ssh" (filename . "^/ssh.*")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

(ptrv/with-library ibuffer-git
  (setq ibuffer-formats
        '((mark modified read-only git-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (git-status 8 8 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gist
(defvar pcache-directory
  (let ((dir (file-name-as-directory (concat ptrv/tmp-dir "pcache"))))
    (make-directory dir t)
    dir))

(ptrv/after gist
  (setq gist-view-gist t)
  (dolist (mode '((processing-mode . "pde")
                  (conf-mode . "desktop")))
    (add-to-list 'gist-supported-modes-alist mode)))

;; A key map for Gisting
(defvar ptrv/gist-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'gist-region-or-buffer)
    (define-key map "l" #'gist-list)
    map)
  "Keymap for Gists.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit
;; newline after 72 chars in magit-log-edit-mode
(ptrv/after magit
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
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; git-messanger
(ptrv/after git-messenger
  (setq git-messenger:show-detail t))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mercurial
(require 'ahg nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; yasnippet
(yas-global-mode 1)
(ptrv/after yasnippet
  (define-key yas-keymap [(tab)] nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [(control tab)] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-TAB")   'yas-next-field-or-maybe-expand)
  (setq yas-prompt-functions '(yas-ido-prompt
                               yas-x-prompt
                               yas-completing-prompt))
  (ptrv/with-library dropdown-list
    (add-to-list 'yas-prompt-functions 'yas-dropdown-prompt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; undo-tree
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; insert-time.el
(ptrv/with-library insert-time
  (setq insert-date-format "%Y-%m-%d")
  (setq insert-time-format "%H:%M:%S"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pomodoro.el
(ptrv/after pomodoro
  (pomodoro-add-to-mode-line)
  (setq pomodoro-sound-player (ptrv/get-default-sound-command))
  (setq pomodoro-break-start-sound (concat ptrv/etc-dir "sounds/alarm.wav"))
  (setq pomodoro-work-start-sound (concat ptrv/etc-dir "sounds/alarm.wav")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sql-mode
;; switch between sqlite3 and spatialite excutable
(ptrv/after sql
  (defun sql-switch-spatialite-sqlite ()
    (interactive)
    (let* ((sqlprog sql-sqlite-program)
           (change (if (string-match "sqlite" sqlprog)
                       (executable-find "spatialite")
                     (executable-find "sqlite3"))))
      (setq sql-sqlite-program change)
      (message "sql-sqlite-program changed to %s" change)))

  (define-key sql-mode-map (kbd "C-c C-p p") 'sql-set-product)
  (define-key sql-mode-map (kbd "C-c C-p i") 'sql-set-sqli-buffer)
  (define-key sql-mode-map (kbd "C-c C-p s") 'sql-switch-spatialite-sqlite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smart-operator
(autoload 'smart-operator-mode "smart-operator" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tea-time
(autoload 'tea-time "tea-time" nil t)
(ptrv/after tea-time
  (setq tea-time-sound (concat ptrv/etc-dir "sounds/alarm.wav"))
  (setq tea-time-sound-command (concat
                                (ptrv/get-default-sound-command) " %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xah lee modes
(autoload 'xmsi-mode "xmsi-math-symbols-input"
  "Load xmsi minor mode for inputting math/Unicode symbols." t)
(eval-after-load "xmsi-math-symbols-input"
  '(progn
     (define-key xmsi-keymap (kbd "S-SPC") 'nil)
     (define-key xmsi-keymap (kbd "C-c C-8") 'xmsi-change-to-symbol)))

;; xub-mode
(autoload 'xub-mode "xub-mode" "Load xub-mode for browsing Unicode." t)
(defalias 'unicode-browser 'xub-mode)

;; display visited file's path as frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; iflipb
(ptrv/after iflipb
  (setq iflipb-ignore-buffers
        '("*Ack-and-a-half*"
          "*Help*"
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
          "SCLang:PostBuffer*"))
  (setq iflipb-wrap-around t))

(global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
(global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Ack and Ag
;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(defvar ptrv/ack-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ack-and-a-half)
    (define-key map (kbd "s") #'ack-and-a-half-same)
    (define-key map (kbd "g") #'ag)
    (define-key map (kbd "r") #'ag-regexp)
    map)
  "Keymap for Ack.")

;; the silver searcher
(ptrv/after ag
  (setq ag-highlight-search t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; edit-server
(ptrv/after edit-server
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))))
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
(edit-server-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; iedit
(setq iedit-toggle-key-default (kbd "C-;"))
(define-key global-map iedit-toggle-key-default 'iedit-mode)
(define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
(define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
(define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smooth-scrolling
(setq smooth-scroll-margin 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; google-this
(google-this-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; popwin
(ptrv/with-library popwin
  (popwin-mode 1)
  (global-set-key (kbd "C-z") popwin:keymap)

  (setq popwin:special-display-config
        '(("*Help*" :height 30 :stick t)
          ("*Completions*" :noselect t)
          ("*compilation*" :noselect t)
          ("*Messages*")
          ("*Occur*" :noselect t)
          ("\\*Slime Description.*" :noselect t :regexp t :height 30)
          ("*magit-commit*" :noselect t :height 30 :width 80 :stick t)
          ("*magit-diff*" :noselect t :height 30 :width 80)
          ("*magit-edit-log*" :noselect t :height 15 :width 80)
          ("*magit-process*" :noselect t :height 15 :width 80)
          ("\\*Slime Inspector.*" :regexp t :height 30)
          ("*Ido Completions*" :noselect t :height 30)
          ;;("*eshell*" :height 20)
          ("\\*ansi-term\\*.*" :regexp t :height 30)
          ("*shell*" :height 30)
          (".*overtone.log" :regexp t :height 30)
          ("*gists*" :height 30)
          ("*sldb.*":regexp t :height 30)
          ("*Gofmt Errors*" :noselect t)
          ("\\*godoc" :regexp t :height 30)
          ("*Shell Command Output*" :noselect t)
          ("*nrepl-error*" :height 20 :stick t)
          ("*nrepl-doc*" :height 20 :stick t)
          ("*nrepl-src*" :height 20 :stick t)
          ("*Kill Ring*" :height 30)
          ("*project-status*" :noselect t)
          ("*pytest*" :noselect t)
          ("*Python*" :stick t)
          ("*Python Doc*" :noselect t)
          ("*jedi:doc*" :noselect t)
          ("*Registers*" :noselect t)
          ("*ielm*" :stick t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; buffer
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

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
    (progn
      (setq unkillable-scratch-buffer-erase t)
      (message "Enable scratch-buffer erase on kill!"))))

(defun unkillable-scratch-buffer ()
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
          #'(lambda ()
              (and (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (save-match-data
                         (looking-at "^#!"))))
                   (not (file-executable-p buffer-file-name))
                   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
                   (message
                    (concat "Saved as script: " buffer-file-name)))))

;; fix whitespace-cleanup
;; http://stackoverflow.com/a/12958498/464831
(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
                                      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;; use ibuffer
(global-set-key [remap list-buffers] 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org
(ptrv/add-auto-mode 'org-mode "\\.org\\'")

(ptrv/after org
  (message "Org config has been loaded !!!")

  (setq org-outline-path-complete-in-steps nil
        org-completion-use-iswitchb nil
        org-completion-use-ido t
        org-log-done t
        org-clock-into-drawer t
        org-src-fontify-natively nil
        org-default-notes-file "~/Dropbox/org/captures.org"
        org-directory "~/Dropbox/org"
        org-agenda-files '("~/Dropbox/org/newgtd.org"
                           "~/Dropbox/org/uni.org"
                           "~/Dropbox/org/master_thesis.org"))

  (setq org-link-mailto-program
        '(browse-url "https://mail.google.com/mail/?view=cm&to=%a&su=%s"))

  (setq org-mobile-directory "~/Dropbox/MobileOrg"
        org-mobile-files '("~/Dropbox/org/newgtd.org"
                           "~/Dropbox/org/uni.org"
                           "~/Dropbox/org/notes.org"
                           "~/Dropbox/org/journal.org"
                           "~/Dropbox/org/master_thesis.org")
        org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")

  ;; yasnippet workaround
  (ptrv/after yasnippet
    (defun yas-org-very-safe-expand ()
      (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

    (defun org-mode-yasnippet-workaround ()
      (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand))
    (add-hook 'org-mode-hook 'org-mode-yasnippet-workaround))

  (defun org-mode-init ()
    (auto-complete-mode -1)
    (turn-off-flyspell))

  (add-hook 'org-mode-hook 'org-mode-init)

  (define-key org-mode-map (kbd "C-c g") 'org-sparse-tree)

  (setq org-agenda-custom-commands
        '(("P" "Projects"
           ((tags "PROJECT")))
          ("H" "Home Lists"
           ((tags "HOME")
            (tags "COMPUTER")
            (tags "DVD")
            (tags "READING")))
          ("U" "Uni"
           ((tags "UNI")))
          ;; ("W" "Work Lists"
          ;;  ((tags "WORK")))
          ("D" "Daily Action List"
           ((agenda "" ((org-agenda-ndays 1)
                        (org-agenda-sorting-strategy
                         '((agenda time-up priority-down tag-up)))
                        (org-deadline-warning-days 0)
                        ))))
          ))

  (setq org-ditaa-jar-path "~/applications/ditaa.jar")
  (setq org-plantuml-jar-path "~/applications/plantuml.jar")

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
     (ditaa . t)
     (dot . t)
     (plantuml . t)
     (gnuplot . t)))

  ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
  (add-to-list 'org-src-lang-modes '("sam" . sam))

  ;; org publish projects file
  (ptrv/after ox
    (load "~/.org-publish-projects.el" 'noerror)))

;; autoload org-bookmark-jump-unhide to fix compile warning
(autoload 'org-bookmark-jump-unhide "org" nil nil)

(ptrv/after calendar
  (setq calendar-week-start-day 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org2blog
(ptrv/with-library org2blog-autoloads
  (ptrv/after org2blog
    (ptrv/after my-secrets
      (load "~/.org-blogs.el" 'noerror))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org-present
(autoload 'org-present "org-present" nil t)
(ptrv/after org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; latex
(load "auctex.el" 'noerror t t)
(load "preview-latex.el" 'noerror t t)

(ptrv/after tex
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
               '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))))

(ptrv/after latex
  (message "LaTeX config has been loaded !!!")

  (dolist (hook '(LaTeX-math-mode
                  reftex-mode
                  auto-fill-mode))
    (add-hook 'LaTeX-mode-hook hook))

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq TeX-command-default "latexmk")))

  ;; clean intermediate files from latexmk
  (dolist (suffix '("\\.fdb_latexmk" "\\.fls"))
    (add-to-list 'LaTeX-clean-intermediate-suffixes suffix))

  (autoload 'info-lookup-add-help "info-look" nil nil)
  (info-lookup-add-help
   :mode 'latex-mode
   :regexp ".*"
   :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
   :doc-spec '(("(latex2e)Concept Index" )
               ("(latex2e)Command Index")))

  (require 'auto-complete-auctex)
  (require 'pstricks))

(ptrv/after reftex
  (message "RefTeX config has been loaded !!!")

  (setq reftex-plug-into-AUCTeX t
        ;; Recommended optimizations
        reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)

  (setq reftex-ref-style-default-list '("Hyperref")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; filetypes
(ptrv/add-auto-mode 'shell-script-mode
  "\\.zsh-template$" "\\.zsh$")

(ptrv/add-auto-mode 'css-mode "\\.css$")
(ptrv/add-auto-mode 'js2-mode "\\.js\\(on\\)?$")
(ptrv/add-auto-mode 'nxml-mode "\\.xml$")

(ptrv/add-auto-mode 'ruby-mode "\\.rb$" "Rakefile$")
(ptrv/add-auto-mode 'yaml-mode
  "\\.yml$" "\\.yaml$" "\\.ya?ml$")

;; Snippets
(ptrv/add-auto-mode 'snippet-mode "snippets/" "\\.yasnippet$")

;; pd-mode
(autoload 'pd-mode "pd-mode" "autoloaded" t)
(ptrv/add-auto-mode 'pd-mode "\\.pat$" "\\.pd$")

;; gitconfig
(ptrv/add-auto-mode 'gitconfig-mode "gitconfig*")

;; cmake
(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(ptrv/add-auto-mode 'cmake-mode "CMakeLists\\.txt\\'" "\\.cmake\\'")

;; desktop-entry-mode
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(ptrv/add-auto-mode 'desktop-entry-mode "\\.desktop\\(\\.in\\)?$")
(add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)

;; glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(ptrv/add-auto-mode 'glsl-mode
  "\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'")

;; IanniX
(ptrv/add-auto-mode 'js-mode "\\.nxscript$")

;; ChucK
(autoload 'chuck-mode "chuck-mode" nil t)
(ptrv/add-auto-mode 'chuck-mode "\\.ck$")

;; arduino
(autoload 'arduino-mode "arduino-mode" nil t)
(ptrv/add-auto-mode 'arduino-mode "\\.ino\\'")

;; arch linux
(ptrv/add-auto-mode 'pkgbuild-mode "/PKGBUILD$")
(ptrv/add-auto-mode 'shell-script-mode "\\.install$")
(ptrv/add-auto-mode 'conf-unix-mode "\\.*rc$")

;; json
(ptrv/add-auto-mode 'json-mode "\\.json$")

;; Carton
(ptrv/add-auto-mode 'emacs-lisp-mode "/Carton$")

;;;; abbrev
(define-abbrev-table 'global-abbrev-table
  '(
    ;; typo corrections
    ("teh" "the")
    ))
(setq save-abbrevs nil)              ; stop asking whether to save newly
                                     ; added abbrev when quitting emacs
(setq-default abbrev-mode t)            ; turn on abbrev mode globally

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; markdown
(ptrv/add-auto-mode 'markdown-mode "\\.md$" "\\.markdown$")

(setq markdown-css-path (concat ptrv/etc-dir "css/markdown.css"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pandoc
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(ptrv/add-auto-mode 'markdown-mode "\\.text$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; golang

(ptrv/after go-mode
  (message "go-mode config has been loaded !!!")

  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH")

  ;; go-lang completion
  (add-to-list 'load-path (concat
                           (car (split-string (getenv "GOPATH") ":"))
                           "/src/github.com/nsf/gocode/emacs"))

  (ptrv/with-library go-autocomplete
    (defface ac-go-mode-candidate-face
      '((t (:background "lightgray" :foreground "navy")))
      "Face for go-autocomplete candidate"
      :group 'auto-complete)
    (defface ac-go-mode-selection-face
      '((t (:background "navy" :foreground "white")))
      "Face for the go-autocomplete selected candidate."
      :group 'auto-complete)
    (setcdr (assoc 'candidate-face ac-source-go)
            'ac-go-mode-candidate-face)
    (setcdr (assoc 'selection-face ac-source-go)
            'ac-go-mode-selection-face))

  ;; (defun go-dot-complete ()
  ;;   "Insert dot and complete code at point."
  ;;   (interactive)
  ;;   (insert ".")
  ;;   (unless (ac-cursor-on-diable-face-p)
  ;;     (auto-complete '(ac-source-go))))

  ;; compile fucntions
  (defun ptrv/go-build ()
    "compile project"
    (interactive)
    (compile "go build"))

  (defun ptrv/go-test ()
    "test project"
    (interactive)
    (compile "go test -v"))

  (defun ptrv/go-chk ()
    "gocheck project"
    (interactive)
    (compile "go test -gocheck.vv"))

  (defun ptrv/go-run ()
    "go run current package"
    (interactive)
    (let (files-list
          go-list-result
          go-list-result-list)
      ;; get package files as list
      (setq go-list-result-list
            (s-split ","
                     (car (process-lines
                           "go" "list" "-f"
                           "{{range .GoFiles}}{{.}},{{end}}"))
                     t))
      ;; escape space in file names
      (setq go-list-result
            (mapcar
             (lambda (x) (s-replace " " "\\ " x)) go-list-result-list))
      (setq files-list (s-join " " go-list-result))
      (compile (concat "go run " files-list))))

  (defun ptrv/go-run-buffer ()
    "go run current buffer"
    (interactive)
    (compile (concat "go run " buffer-file-name)))

  (defun ptrv/go-mode-init ()
    (make-local-variable 'before-save-hook)
    (setq before-save-hook 'gofmt-before-save)
    (hs-minor-mode 1))

  (add-hook 'go-mode-hook 'ptrv/go-mode-init)

  (defvar ptrv/go-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "c" 'ptrv/go-run)
      (define-key map "r" 'ptrv/go-run-buffer)
      (define-key map "b" 'ptrv/go-build)
      (define-key map "t" 'ptrv/go-test)
      (define-key map "g" 'ptrv/go-chk)
      map)
    "Keymap for some custom go-mode fns.")

  (let ((map go-mode-map))
    (define-key map (kbd "M-.") 'godef-jump)
    (define-key map (kbd "C-c i") 'go-goto-imports)
    (define-key map (kbd "C-c C-r") 'go-remove-unused-imports)
    (define-key map (kbd "C-c C-p") 'go-create-package)
    (define-key map (kbd "C-c C-c") ptrv/go-mode-map))

  (ptrv/after auto-complete-config
    (define-key go-mode-map (kbd ".") 'ptrv/ac-dot-complete))

  ;; flycheck support
  (add-to-list 'load-path (concat
                           (car (split-string (getenv "GOPATH") ":"))
                           "/src/github.com/dougm/goflymake"))

  (ptrv/with-library go-flycheck
    (setq goflymake-debug nil)

    (ptrv/after flycheck
      (flycheck-declare-checker go
        "A Go syntax and style checker using the gofmt utility. "
        :command '("gofmt" source)
        :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
        :modes 'go-mode
        :next-checkers '((no-errors . go-goflymake)))
      (add-to-list 'flycheck-checkers 'go t)
      ;; remove go-goflymake from begin of list and add it to the end
      (setq flycheck-checkers (remove 'go-goflymake flycheck-checkers))
      (add-to-list 'flycheck-checkers 'go-goflymake t)))

  (when (executable-find "errcheck")
    (autoload 'go-errcheck "go-errcheck" nil t)
    (define-key ptrv/go-mode-map "e" 'go-errcheck)))

(defvar ptrv/go-default-namespace "github.com/ptrv")

(defun ptrv/go-create-package (name &optional arg)
  "Create a new sketch with NAME under GOPATH src folder.

If ARG is not nil, create package in current directory"
  (interactive "sInsert new package name: \nP")
  (let ((name (remove ?\s name))
        (root-dir (concat (car (split-string (getenv "GOPATH") ":"))
                          "/src/" ptrv/go-default-namespace)))
    (if (not (string-equal "" name))
        (progn
          (unless arg
            (setq name (concat (file-name-as-directory root-dir) name)))
          (make-directory name)
          (find-file (concat (file-name-as-directory name) "main.go")))
      (error "Please insert a package name"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xml
(ptrv/add-auto-mode 'nxml-mode "\\.gpx$")

(ptrv/after nxml-mode
  ;; make nxml outline work with gpx files
  (defun gpx-setup ()
    (when (and (stringp buffer-file-name)
               (string-match "\\.gpx\\'" buffer-file-name))
      (make-local-variable 'nxml-section-element-name-regexp)
      (setq nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
      (make-local-variable 'nxml-heading-element-name-regexp)
      (setq nxml-heading-element-name-regexp "name\\|time")
      ))
  (add-hook 'nxml-mode-hook 'gpx-setup)

  (setq nxml-slash-auto-complete-flag t
        nxml-sexp-element-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; erc
(ptrv/after my-secrets
  (defun erc-connect ()
    "Start up erc and connect to freedonde"
    (interactive)
    (erc :server "irc.freenode.net"
         :full-name "Peter V."
         :port 6667
         :nick freenode-user
         ))
  (ptrv/after erc
    (erc-services-mode 1)
    (setq erc-prompt-for-nickserv-password nil)
    (setq erc-nickserv-passwords
          `((freenode ((,freenode-user . ,freenode-pass)))
            (oftc ((,oftc-user . ,oftc-pass)))))

    ;;IRC
    (erc-autojoin-mode 1)
    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs")))

    (cond ((string= system-name "alderaan")
           (setq erc-autojoin-channels-alist
                 (list (append (car erc-autojoin-channels-alist)
                               '("#supercollider" "#archlinux")))))
          ((string= system-name "anoth")
           (setq erc-autojoin-channels-alist
                 (list (append (car erc-autojoin-channels-alist)
                               '("#supercollider" "#archlinux")))))
          ;; (t (setq erc-autojoin-channels-alist
          ;;          '(("freenode.net" "#emacs" "#clojure" "overtone"))))
          )

    (setq erc-keywords `(,freenode-user))
    (erc-match-mode)

    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))))

(make-variable-buffer-local 'erc-fill-column)
(ptrv/after erc
  ;;change wrap width when window is resized
  (add-hook 'window-configuration-change-hook
            '(lambda ()
               (save-excursion
                 (walk-windows
                  (lambda (w)
                    (let ((buffer (window-buffer w)))
                      (set-buffer buffer)
                      (when (eq major-mode 'erc-mode)
                        (setq erc-fill-column (- (window-width w) 2))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; faust-mode
(ptrv/add-auto-mode 'faust-mode "\\.dsp$")
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Synth-A-Modeler mode
(ptrv/add-auto-mode 'sam-mode "\\.mdl$")
(autoload 'sam-mode "sam-mode" "Synth-A-Modeler editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; editing
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
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;(global-subword-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; move-text
(move-text-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file commands
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
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (:else
      (delete-file filename)
      (kill-buffer)))))

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun ptrv/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defvar ptrv/file-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'ptrv/open-with)
    (define-key map "d" #'ptrv/launch-directory)
    (define-key map "R" #'ptrv/rename-current-buffer-file)
    (define-key map "D" #'ptrv/delete-file-and-buffer)
    (define-key map "w" #'ptrv/copy-file-name-to-clipboard)
    (define-key map "i" #'ptrv/find-user-init-file)
    (define-key map (kbd "b i") #'ptrv/byte-recompile-init)
    (define-key map (kbd "b s") #'ptrv/byte-recompile-site-lisp)
    (define-key map (kbd "b e") #'ptrv/byte-recompile-elpa)
    (define-key map (kbd "b h") #'ptrv/byte-recompile-home)
    (define-key map (kbd "b b") #'emacs-lisp-byte-compile-and-load)
    map)
  "Keymap for file functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; projectile
(ptrv/after projectile
  (dolist (file '(".ropeproject" "setup.py"))
    (add-to-list 'projectile-project-root-files file t)))
(setq projectile-known-projects-file (concat
                                      ptrv/tmp-dir
                                      "projectile-bookmarks.eld")
      projectile-cache-file (concat ptrv/tmp-dir "projectile.cache"))
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ffip
(setq ffip-project-file '(".git" ".hg" ".ropeproject" "setup.py"))
(define-key ptrv/file-commands-map "f" 'ffip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; processing
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(autoload 'processing-snippets-initialize "processing-mode" nil nil nil)
(autoload 'processing-find-sketch "processing-mode" nil t)
(ptrv/add-auto-mode 'processing-mode "\\.pde$")

(ptrv/after processing-mode
  (message "Processing config has been loaded !!!")

  (ptrv/after yasnippet
    (processing-snippets-initialize))

  (cond (*is-mac*
         (setq processing-location "/usr/bin/processing-java")
         (setq processing-application-dir
               "/Applications/Processing.app/Contents/Resources/Java")
         (setq processing-sketch-dir "~/Documents/Processing"))
        (*is-linux*
         (setq processing-location "~/applications/processing-2.0/processing-java")
         (setq processing-application-dir "~/applications/processing-2.0")
         (setq processing-sketch-dir "~/processing_sketches_v2")))

  (defun processing-mode-init ()
    (make-local-variable 'ac-sources)
    (setq ac-sources '(ac-source-dictionary
                       ac-source-yasnippet
                       ac-source-words-in-buffer))
    (make-local-variable 'ac-user-dictionary)
    (setq ac-user-dictionary (append processing-functions
                                     processing-builtins
                                     processing-constants)))

  (add-to-list 'ac-modes 'processing-mode)
  (add-hook 'processing-mode-hook 'processing-mode-init)
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
                (progn
                  (with-current-buffer htmlbuf
                    (clipboard-kill-ring-save (point-min) (point-max)))
                  (kill-buffer htmlbuf)
                  (message "Copied as HTML to clipboard")))))
        (message (concat "Copy as HTML failed, because current "
                         "buffer is not a Processing buffer."))))
    (define-key processing-mode-map (kbd "C-c C-p H") 'processing-copy-as-html)
    (easy-menu-add-item processing-mode-menu nil (list "---"))
    (easy-menu-add-item processing-mode-menu nil
                        ["Copy as HTML" processing-copy-as-html
                         :help "Copy buffer or region as HTML to clipboard"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; flycheck
(unless (and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
  (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
  (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
  (add-to-list 'debug-ignored-errors "\\`Configured syntax checker .* cannot be used\\'"))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable flycheck for some modes
(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook #'(lambda () (flycheck-mode -1))))

(ptrv/after flycheck
  (setq flycheck-highlighting-mode 'lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; flymake
(defvar flymake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'flymake-goto-next-error)
    (define-key map (kbd "M-p") 'flymake-goto-prev-error)
    map))
(add-to-list 'minor-mode-map-alist `(flymake-mode . ,flymake-mode-map) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hideshow
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(dolist (x '(emacs-lisp lisp java perl sh python))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'hs-minor-mode))

(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c @ h")   'hs-hide-block)
        (define-key map (kbd "C-c @ H")   'hs-show-block)
        (define-key map (kbd "C-c @ s")	  'hs-hide-all)
        (define-key map (kbd "C-c @ S")	  'hs-show-all)
        (define-key map (kbd "C-c @ l")   'hs-hide-level)
        (define-key map (kbd "C-c @ c")   'hs-toggle-hiding)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
        map))

;; https://github.com/Hawstein/my-emacs/blob/master/_emacs/hs-minor-mode-settings.el
(setq hs-isearch-open t)

;; (global-set-key (kbd "<f11>") 'toggle-fold)
(global-set-key (kbd "<f11>") 'hs-toggle-hiding)
(global-set-key (kbd "S-<f11>") 'toggle-fold-all)

(defvar hs-hide-all nil "Current state of hideshow for toggling all.")
(make-local-variable 'hs-hide-all)

(defun hs-toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq hs-hide-all (not hs-hide-all))
  (if hs-hide-all
      (hs-hide-all)
    (hs-show-all)))

(defvar fold-all-fun nil "Function to fold all.")
(make-variable-buffer-local 'fold-all-fun)
(defvar fold-fun nil "Function to fold.")
(make-variable-buffer-local 'fold-fun)

(defun toggle-fold-all ()
  "Toggle fold all."
  (interactive)
  (if fold-all-fun
      (call-interactively fold-all-fun)
    (hs-toggle-hiding-all)))

(defun toggle-fold ()
  "Toggle fold."
  (interactive)
  (if fold-fun
      (call-interactively fold-fun)
    (hs-toggle-hiding)))

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

(defadvice goto-line-with-feedback (after expand-after-goto-line-with-feedback
                                          activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; autopair
(autopair-global-mode 1)

(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clean-mode-line
(ptrv/after auto-complete (diminish 'auto-complete-mode " α"))
(ptrv/after yasnippet (diminish 'yas-minor-mode " γ"))
(ptrv/after paredit (diminish 'paredit-mode " Φ"))
(ptrv/after eldoc (diminish 'eldoc-mode))
(ptrv/after abbrev (diminish 'abbrev-mode))
(ptrv/after undo-tree (diminish 'undo-tree-mode " τ"))
(ptrv/after elisp-slime-nav (diminish 'elisp-slime-nav-mode " δ"))
(ptrv/after nrepl (diminish 'nrepl-interaction-mode " ηζ"))
(ptrv/after simple (diminish 'auto-fill-function " φ"))
(ptrv/after autopair (diminish 'autopair-mode))
(ptrv/after projectile (diminish 'projectile-mode))
(ptrv/after kibit-mode (diminish 'kibit-mode " κ"))
(ptrv/after google-this (diminish 'google-this-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "nrepl" nrepl-mode "ηζ")
(rename-modeline "clojure-mode" clojure-mode "λ")
(rename-modeline "python" python-mode "Py")
(rename-modeline "lisp-mode" emacs-lisp-mode "EL")
(rename-modeline "markdown-mode" markdown-mode "md")
(rename-modeline "processing-mode" processing-mode "P5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; osx
(when *is-mac*
  (ptrv/after ns-win
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil))

  (when *is-cocoa-emacs*
    (set-frame-font "Inconsolata-16" nil t)
    (set-frame-size (selected-frame) 110 46)
    (set-frame-position (selected-frame) 370 24))

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
  (let ((gls (executable-find "gls"))
        (gfind (executable-find "gfind")))
    (if gls
        (setq insert-directory-program gls)
      (message "GNU coreutils not found. Install coreutils with homebrew."))
    (when gfind
      (setq find-program gfind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; linux
(when *is-linux*
  (set-frame-font "Inconsolata-12" nil t)
  (ptrv/after eshell
    (autoload 'pcomplete/apt-get "pcmpl-apt" nil nil))

  (defun setup-frame-hook (frame)
    ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
    ;;(run-with-idle-timer 0.2 nil 'toggle-fullscreen)
    )
  (add-hook 'after-make-frame-functions 'setup-frame-hook)

  ;; erc notification
  (ptrv/after erc
    (defun my-notify-erc (match-type nickuserhost message)
      "Notify when a message is received."
      (unless (posix-string-match "^\\** *Users on #" message)
        (notify (format "%s in %s"
                        ;; Username of sender
                        (car (split-string nickuserhost "!"))
                        ;; Channel
                        (or (erc-default-target) "#unknown"))
                ;; Remove duplicate spaces
                (replace-regexp-in-string " +" " " message)
                ;; :icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
                :timeout -1)))

    (add-hook 'erc-text-matched-hook 'my-notify-erc))

  ;; typeriter-mode
  (autoload 'typewriter-mode "typewriter-mode" nil t)
  (ptrv/after typewriter-mode
    (setq typewriter-play-command (concat
                                   (ptrv/get-default-sound-command)
                                   " %s"))
    (setq typewriter-sound-default (concat
                                    ptrv/etc-dir
                                    "sounds/9744__horn__typewriter.wav"))
    (setq typewriter-sound-end (concat
                                ptrv/etc-dir
                                "sounds/eol-bell.wav"))
    (setq typewriter-sound-return (concat
                                   ptrv/etc-dir
                                   "sounds/carriage-return.wav"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; superollider
(ptrv/after w3m
  (define-key w3m-mode-map [left] 'backward-char)
  (define-key w3m-mode-map [right] 'forward-char)
  (define-key w3m-mode-map [up] 'previous-line)
  (define-key w3m-mode-map [down] 'next-line)
  (setq w3m-auto-show 1)
  (setq w3m-key-binding 'info)
  (setq w3m-pop-up-frames t)
  (setq w3m-pop-up-windows nil))

(defun ptrv/sclang-mode-loader ()
  (unless (featurep 'sclang)
    (require 'sclang)
    (ptrv/after sclang
      (sclang-mode)
      (ptrv/sclang-mode-loader--remove))))
(defun ptrv/sclang-mode-loader--remove ()
  (delete (rassq 'ptrv/sclang-mode-loader auto-mode-alist)
          auto-mode-alist))
(ptrv/add-auto-mode 'ptrv/sclang-mode-loader "\\.\\(sc\\|scd\\)$")

(defun ptrv/sclang ()
  (interactive)
  (if (require 'sclang nil t)
      (progn
        (ptrv/after sclang
          (sclang-start))
        (ptrv/sclang-mode-loader--remove))
    (message "SCLang is not installed!")))

(ptrv/after sclang
  (message "sclang config has been loaded !!!")
  (setq sclang-auto-scroll-post-buffer nil
        sclang-eval-line-forward nil
        ;;sclang-help-path '("~/.local/share/SuperCollider/Help")
        sclang-library-configuration-file "~/.sclang.cfg"
        sclang-runtime-directory "~/scwork/"
        sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")

  (defun sclang-mode-untabify ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

  (defun supercollider-init ()
    (add-to-list 'ac-modes 'sclang-mode)
    (make-local-variable 'ac-user-dictionary-files)
    (add-to-list 'ac-user-dictionary-files "~/.sc_completion")

    ;; (add-to-list 'ac-user-dictionary-files
    ;;              "~/.local/share/SuperCollider/sclang_completion_dict")
    (yas-minor-mode 1)
    (make-local-variable 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'sclang-mode-untabify)

    ;; set buffer local keymap to set TAB for jumping to next button in
    ;; post window when using ext-scel's collapsible post window text.
    (when (string= (buffer-name) sclang-post-buffer)
      (use-local-map (copy-keymap sclang-mode-map))
      (local-set-key [?\t] 'forward-button)
      (local-set-key [backtab] 'backward-button)))

  (add-hook 'sclang-mode-hook 'supercollider-init)

  (define-key sclang-mode-map (kbd "C-c ]") 'sclang-pop-definition-mark)
  ;; Raise all supercollider windows.
  (define-key sclang-mode-map (kbd "C-c F")
    (lambda ()
      (interactive)
      (sclang-eval-string "Window.allWindows.do(_.front);")))
  (define-key sclang-server-key-map [?l]
    (lambda ()
      (interactive)
      (sclang-eval-string "Server.default.meter;")))
  (define-key sclang-server-key-map [?s]
    (lambda ()
      (interactive)
      (sclang-eval-string "Server.default.scope(numChannels: 2);")))
  (define-key sclang-server-key-map [?h]
    (lambda ()
      (interactive)
      (sclang-eval-string "HelperWindow.new;")))
  (define-key sclang-mode-map (kbd "s-.") 'sclang-main-stop)
  (define-key sclang-mode-map (kbd "<s-return>") 'sclang-eval-region-or-line)

  ;; snippets
  (autoload 'sclang-snippets-initialize "sclang-snippets" nil nil)
  (ptrv/after yasnippet
    (sclang-snippets-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elpy
(ptrv/after python
  (setq elpy-default-minor-modes '(eldoc-mode
                                   highlight-indentation-mode
                                   yas-minor-mode
                                   auto-complete-mode))
  (elpy-enable t)

  (add-hook 'elpy-mode-hook #'(lambda ()
                                (when (file-remote-p (buffer-file-name))
                                  (elpy-disable)))))

(ptrv/after elpy
  (let ((map elpy-mode-map))
    (define-key map (kbd "C-c C-f") nil)
    (define-key map (kbd "C-c C-j") nil)
    (define-key map (kbd "C-c C-n") nil)
    (define-key map (kbd "C-c C-p") nil)
    (define-key map (kbd "C-c C-t") nil)
    (define-key map (kbd "C-c C-t n") 'elpy-test))

  (ptrv/after auto-complete-config
    (define-key elpy-mode-map "." 'ptrv/ac-dot-complete))

  (setq python-check-command "flake8")
  (add-hook 'python-mode-hook 'elpy-initialize-local-variables)

  (let ((elpy-snippets (concat (file-name-directory (locate-library "elpy"))
                               "snippets/")))
    (when (and (file-exists-p elpy-snippets)
               (fboundp 'yas-snippet-dirs))
      (add-to-list 'yas-snippet-dirs elpy-snippets t)
      (yas-load-directory elpy-snippets t)))

  (defun elpy-use-ipython-pylab ()
    "Set defaults to use IPython instead of the standard interpreter."
    (interactive)
    (unless (boundp 'python-python-command)
      (elpy-use-ipython)
      (setq python-shell-interpreter-args "--pylab"))))

(ptrv/after highlight-indentation
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; python
(ptrv/after python
  (exec-path-from-shell-copy-env "PYTHONPATH")

  ;; pytest
  (autoload 'pytest-all "pytest" nil t)
  (autoload 'pytest-module "pytest" nil t)
  (autoload 'pytest-one "pytest" nil t)
  (autoload 'pytest-directory "pytest" nil t)
  (setq pytest-global-name "py.test")
  (let ((map python-mode-map))
    (define-key map (kbd "C-c C-t a") 'pytest-all)
    (define-key map (kbd "C-c C-t m") 'pytest-module)
    (define-key map (kbd "C-c C-t o") 'pytest-one)
    (define-key map (kbd "C-c C-t d") 'pytest-directory)
    (define-key map (kbd "C-c C-t p a") 'pytest-pdb-all)
    (define-key map (kbd "C-c C-t p m") 'pytest-pdb-module)
    (define-key map (kbd "C-c C-t p o") 'pytest-pdb-one))

  ;; info
  (autoload 'info-lookup-add-help "info-look" nil nil)
  (info-lookup-add-help
   :mode 'python-mode
   :regexp "[[:alnum:]_]+"
   :doc-spec
   '(("(python)Index" nil "")))

  ;; pylint
  (add-hook 'python-mode-hook 'pylint-add-menu-items)
  (add-hook 'python-mode-hook 'pylint-add-key-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cc-mode
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

;; Hook auto-complete into clang
(ptrv/after cc-mode
  (message "cc-mode config has been loaded !!!")
  (ptrv/with-library auto-complete-clang-async
    (setq ac-clang-complete-executable
          (locate-user-emacs-file "site-lisp/emacs-clang-complete-async/clang-complete"))
    (when (not (file-exists-p ac-clang-complete-executable))
      (warn "The clang-complete executable doesn't exist"))
    ;; Add Qt4 includes to load path if installed
    (when (file-exists-p "/usr/include/qt4")
      (setq-default ac-clang-cflags
                    (mapcar (lambda (f) (concat "-I" f))
                            (directory-files "/usr/include/qt4" t "Qt\\w+"))))

    (defun ptrv/clang-complete-init ()
      (unless (string-match ".*flycheck.*" buffer-file-name)
        (setq ac-sources '(ac-source-clang-async))
        (ac-clang-launch-completion-process)))
    (dolist (hook '(c-mode-hook c++-mode-hook))
      (add-hook hook 'ptrv/clang-complete-init)))

  (defun ptrv/cc-mode-init ()
    (setq c-basic-offset 4
          ;;c-indent-level 4
          c-default-style "bsd"
          indent-tabs-mode nil)
    (local-set-key  (kbd "C-c o") 'ff-find-other-file))

  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook 'ptrv/cc-mode-init))

  (add-hook 'c-mode-common-hook 'linum-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; x11
(when *is-x11*
  ;; Maximise the Emacs window
  (defun toggle-fullscreen ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

  (cond ((and (<= (x-display-pixel-width) 1280)
              (<= (x-display-pixel-height) 800))
         ;; (toggle-fullscreen)
         )
        ((and (eq (x-display-pixel-width) 1680)
              (eq (x-display-pixel-height) 1050))
         ;; (set-frame-size (selected-frame) 110 60)
         (set-frame-size (selected-frame) 130 55)
         (set-frame-position (selected-frame) 500 30))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key-chord
(key-chord-mode 1)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-define-global "KK" 'winner-undo)
(key-chord-define-global "LL" 'winner-redo)
(key-chord-define-global "BB" 'ido-switch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bindings
(global-set-key (kbd "C-x f") 'ptrv/ido-recentf-open)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key [f5] 'refresh-file)

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'delete-other-windows)

(global-set-key (kbd "M-C-y") 'kill-ring-search)

;;diff shortcuts
(defvar ptrv/diff-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'diff)
    (define-key map "f" 'diff-buffer-with-file)
    map)
  "Keymap for diff commands.")

(defvar ptrv/windows-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'swap-windows)
    (define-key map "r" 'rotate-windows)
    (define-key map "." (lambda () (interactive) (shrink-window-horizontally 4)))
    (define-key map "," (lambda () (interactive) (enlarge-window-horizontally 4)))
    (define-key map (kbd "<down>") (lambda () (interactive) (enlarge-window -4)))
    (define-key map (kbd "<up>") (lambda () (interactive) (enlarge-window 4)))
    (define-key map "b" 'winner-undo)
    (define-key map "f" 'winner-redo)
    map)
  "Keymap for window manipulation")

;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-C-%") 'query-replace)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(global-set-key "\C-m" 'newline-and-indent)

(global-set-key (kbd "M-;") 'comment-dwim-line)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region-below)
(global-set-key (kbd "C-S-M-d") 'duplicate-line-below-comment)

(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "M-j") (lambda ()
                              (interactive)
                              (join-line -1)))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "C-M-z") 'indent-defun)

(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

(global-set-key (kbd "C-h F") 'find-function)
(global-set-key (kbd "C-h V") 'find-variable)

;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
(global-set-key (kbd "C-x O") #'(lambda ()
                                  (interactive)
                                  (other-window -1)))

;; registers
(global-set-key (kbd "C-x r T") 'string-insert-rectangle)
(global-set-key (kbd "C-x r v") 'ptrv/list-registers)

(global-set-key (kbd "C-M-=") 'increment-number-at-point)
(global-set-key (kbd "C-M--") 'decrement-number-at-point)

;; Keymap for characters following C-c
(let ((map mode-specific-map))
  (define-key map "A" 'org-agenda)
  (define-key map "B" 'org-iswitchb)
  (define-key map "C" 'org-capture)
  (define-key map "G" ptrv/gist-map)
  (define-key map "R" 'refresh-file)
  (define-key map "a" ptrv/ack-map)
  (define-key map "d" ptrv/diff-map)
  (define-key map "f" ptrv/file-commands-map)
  (define-key map "g" 'magit-status)
  (define-key map "l" 'org-store-link)
  (define-key map "q" 'exit-emacs-client)
  (define-key map "r" 'revert-buffer)
  (define-key map "t" 'ptrv/eshell-or-restore)
  (define-key map "v" 'halve-other-window-height)
  (define-key map "w" ptrv/windows-map)
  (define-key map "y" 'ptrv/display-yank-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defuns
(defun ptrv/ido-recentf-open ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t nil))

(defun ptrv/display-yank-menu ()
  "Open yank-menu popup."
  (interactive)
  (popup-menu 'yank-menu))

;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.

If no region is selected and current line is not blank and we are
not at the end of the line, then comment current line. Replaces
default behaviour of comment-dwim, when it inserts comment at the
end of the line."
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
    (progn
      (window-configuration-to-register :eshell-fullscreen)
      (eshell)
      (delete-other-windows))))

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
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun ptrv/byte-recompile-site-lisp ()
  (interactive)
  (byte-recompile-directory (locate-user-emacs-file "site-lisp") 0))

(defun ptrv/byte-recompile-elpa ()
  (interactive)
  (when (boundp 'package-user-dir)
    (byte-recompile-directory package-user-dir 0)))

(defun ptrv/byte-recompile-init ()
  (interactive)
  (byte-recompile-file (locate-user-emacs-file "init.el") t 0))

(defun ptrv/byte-recompile-home ()
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; Recreate scratch buffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
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

;; http://www.emacswiki.org/emacs/basic-edit-toolkit.el
(defun duplicate-line-or-region-above (&optional reverse)
  "Duplicate current line or region above.
By default, duplicate current line above.
If mark is activate, duplicate region lines above.
Default duplicate above, unless option REVERSE is non-nil."
  (interactive)
  (let ((origianl-column (current-column))
        duplicate-content)
    (if mark-active
        ;; If mark active.
        (let ((region-start-pos (region-beginning))
              (region-end-pos (region-end)))
          ;; Set duplicate start line position.
          (setq region-start-pos (progn
                                   (goto-char region-start-pos)
                                   (line-beginning-position)))
          ;; Set duplicate end line position.
          (setq region-end-pos (progn
                                 (goto-char region-end-pos)
                                 (line-end-position)))
          ;; Get duplicate content.
          (setq duplicate-content (buffer-substring region-start-pos region-end-pos))
          (if reverse
              ;; Go to next line after duplicate end position.
              (progn
                (goto-char region-end-pos)
                (forward-line +1))
            ;; Otherwise go to duplicate start position.
            (goto-char region-start-pos)))
      ;; Otherwise set duplicate content equal current line.
      (setq duplicate-content (buffer-substring
                               (line-beginning-position)
                               (line-end-position)))
      ;; Just move next line when `reverse' is non-nil.
      (and reverse (forward-line 1))
      ;; Move to beginning of line.
      (beginning-of-line))
    ;; Open one line.
    (open-line 1)
    ;; Insert duplicate content and revert column.
    (insert duplicate-content)
    (move-to-column origianl-column t)))

(defun duplicate-line-or-region-below ()
  "Duplicate current line or region below.
By default, duplicate current line below.
If mark is activate, duplicate region lines below."
  (interactive)
  (duplicate-line-or-region-above t))

(defun duplicate-line-above-comment (&optional reverse)
  "Duplicate current line above, and comment current line."
  (interactive)
  (if reverse
      (duplicate-line-or-region-below)
    (duplicate-line-or-region-above))
  (save-excursion
    (if reverse
        (forward-line -1)
      (forward-line +1))
    (comment-or-uncomment-region+)))

(defun duplicate-line-below-comment ()
  "Duplicate current line below, and comment current line."
  (interactive)
  (duplicate-line-above-comment t))

(defun comment-or-uncomment-region+ ()
  "This function is to comment or uncomment a line or a region."
  (interactive)
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (setq beg (line-beginning-position))
      (setq end (line-end-position)))
    (save-excursion
      (comment-or-uncomment-region beg end))))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun exit-emacs-client ()
  "consistent exit emacsclient.
   if not in emacs client, echo a message in minibuffer, don't
   exit emacs. if in server mode and editing file, do C-x #
   server-edit else do C-x 5 0 delete-frame"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (delete-frame)))

;; http://whattheemacsd.com/editing-defuns.el-01.html
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
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
        (linum-mode -1)))))

(defun toggle-window-split ()
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
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(defun xml-format ()
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
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; http://emacsredux.com/blog/2013/03/28/indent-defun/
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun ptrv/user-first-name ()
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))
(defun ptrv/user-first-name-p ()
  (not (string-equal "" (ptrv/user-first-name))))

;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (enable-paredit-mode)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

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
  (save-excursion
    (save-match-data
      (or (looking-at "[0123456789]")
          (error "No number at point"))
      (replace-match
       (number-to-string
        (mod (funcall change (string-to-number (match-string 0))) 10))))))
(defun increment-number-at-point ()
  (interactive)
  (ptrv/change-number-at-point '1+))
(defun decrement-number-at-point ()
  (interactive)
  (ptrv/change-number-at-point '1-))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; custom settings
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; local settings
(load (locate-user-emacs-file "locals.el") 'noerror)
(unless (require 'my-secrets "~/.secrets.gpg" t)
  (warn "Could not load secrets file!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; welcome-message stuff
(defvar ptrv/welcome-messages
  (if (ptrv/user-first-name-p)
      (list (concat "Hello " (ptrv/user-first-name) ", somewhere in the world the sun is shining for you right now.")
            (concat "Hello " (ptrv/user-first-name) ", it's lovely to see you again. I do hope that you're well.")
            (concat (ptrv/user-first-name) ", turn your head towards the sun and the shadows will fall behind you."))
    (list  "Hello, somewhere in the world the sun is shining for you right now."
           "Hello, it's lovely to see you again. I do hope that you're well."
           "Turn your head towards the sun and the shadows will fall behind you.")))

(defun ptrv/welcome-message ()
  (nth (random (length ptrv/welcome-messages)) ptrv/welcome-messages))

(setq initial-scratch-message (concat ";;
;; Emacs on " system-name " [" (symbol-name system-type) "]
;;
;; " (ptrv/welcome-message) "

"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el ends here
