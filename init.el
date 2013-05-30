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
(defconst *is-linux* (eq system-type 'gnu/linux))

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

;; Add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

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

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist
    (project (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (and (file-directory-p project)
             (not (string-match "_extras" project)))
    (add-to-list 'load-path project)))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(load "~/.emacs-locals.el" 'noerror)
(require 'my-secrets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; macros
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
;;;; backup
(setq auto-save-list-file-name
      (concat ptrv-autosaves-dir "autosave-list"))
(setq auto-save-file-name-transforms
      `((".*" ,(concat ptrv-autosaves-dir "\\1") t)))
(setq backup-directory-alist
      `((".*" . ,ptrv-backups-dir)))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package
(package-initialize)
(require 'carton)
(carton-setup dotfiles-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PATH
(after 'exec-path-from-shell-autoloads
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; builtins
;; default browser
(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; follow version controlled symlinks automatically
(setq vc-follow-symlinks t)
(setq compilation-scroll-output t)

;; disabled commands
(put 'downcase-region 'disabled nil)
(put 'updacase-region 'disabled nil)

(auto-insert-mode 1)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

(set-default 'fill-column 72)
(setq ring-bell-function 'ignore)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq recentf-save-file (concat ptrv-tmp-dir "recentf")
      recentf-max-saved-items 200)
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
        (message "Aborting")))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat ptrv-tmp-dir "places"))

;;enable cua-mode for rectangular selections
;; (cua-mode 1)
(setq cua-enable-cua-keys nil)

(windmove-default-keybindings)

(when (fboundp 'winner-mode)
      (winner-mode 1))

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t)

;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)

;;default to unified diffs
(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq x-select-enable-clipboard t)

;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defun ptrv-cleanup-whitespace ()
  (let ((whitespace-style '(trailing empty)) )
    (whitespace-cleanup)))
(add-hook 'before-save-hook 'ptrv-cleanup-whitespace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat ptrv-tmp-dir "savehist"))
(savehist-mode t)

(setq completion-cycle-threshold 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; spelling
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(after 'flyspell
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
        try-complete-lisp-symbol
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; look-and-feel
(setq column-number-mode t)
(global-hl-line-mode 1)

(defvar ptrv-themes-dir (concat dotfiles-dir "themes"))
(add-to-list 'load-path ptrv-themes-dir)
(autoload 'color-theme-gandalf-ptrv "gandalf-ptrv" nil nil)
(color-theme-gandalf-ptrv)

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
;;;; nyan-mode
(autoload 'nyan-mode "nyan-mode" nil t)
(setq nyan-bar-length 16)
(nyan-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ido
(setq ido-max-directory-size 100000)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-max-prospects 10
      ido-default-file-method 'selected-window)
(setq ido-save-directory-list-file (concat ptrv-tmp-dir "ido.last"))
(icomplete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ido-ubiquitous
(after 'ido-ubiquitous-autoloads
  (ido-ubiquitous-mode 1)
  (ido-ubiquitous-disable-in erc-iswitchb)

  (add-to-list 'ido-ubiquitous-command-exceptions 'sh-set-shell)
  (add-to-list 'ido-ubiquitous-command-exceptions 'ispell-change-dictionary)
  (add-to-list 'ido-ubiquitous-command-exceptions 'add-dir-local-variable)
  (add-to-list 'ido-ubiquitous-command-exceptions 'ahg-do-command)
  ;;(add-to-list 'ido-ubiquitous-command-exceptions 'godoc)

  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))

  ;;(ido-ubiquitous-use-new-completing-read webjump 'webjump)
  ;; (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
  (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smex
(after 'smex-autoloads
  (setq smex-save-file (concat ptrv-tmp-dir "smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eshell
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; paredit
(after 'paredit-autoloads
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)
  (dolist (x '(scheme emacs-lisp lisp clojure))
    (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode))
  (after 'paredit
    ;; need a binding that works in the terminal
    (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; rainbow-delimiters
(after 'rainbow-delimiters-autoloads
  (dolist (x '(scheme emacs-lisp lisp clojure))
    (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(after 'mic-paren-autoloads
  (paren-activate))

(after 'nrepl-eval-sexp-fu-autoloads
  (require 'highlight)
  (require 'nrepl-eval-sexp-fu)
  (setq nrepl-eval-sexp-fu-flash-duration 0.5))

(after 'lexbind-mode-autoloads
  (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clojure
(after 'find-file-in-project
  (add-to-list 'ffip-patterns "*.clj"))

(after 'clojure-mode
  (message "clojure config has been loaded !!!")

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

  (after 'kibit-mode
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

(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 4clojure
(autoload '4clojure-problem "four-clj" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; complete
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(setq ac-comphist-file (concat ptrv-tmp-dir "ac-comphist.dat"))

(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.8)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-ignore-case nil)
(setq ac-candidate-menu-min 0)
(setq ac-auto-start nil)

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

(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
;;(define-key ac-completing-map "\r" 'nil)
(ac-set-trigger-key "TAB")

;; complete on dot
(defun ac-dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (unless (ac-cursor-on-diable-face-p)
    (auto-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tramp
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-persistency-file-name (concat ptrv-tmp-dir "tramp"))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
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
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

(with-library 'ibuffer-git
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
(after 'gist-autoloads
  (defvar pcache-directory
    (let ((dir (file-name-as-directory (concat ptrv-tmp-dir "pcache"))))
      (make-directory dir t)
      dir))

  (setq gist-view-gist t)
  (after 'gist
    (add-to-list 'gist-supported-modes-alist '(processing-mode . "pde"))
    (add-to-list 'gist-supported-modes-alist '(conf-mode . "desktop"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; git-gutter
(after 'git-gutter-autoloads
  (setq git-gutter:window-width 2)
  ;;(global-git-gutter-mode t)
  (setq git-gutter:lighter " G-+")
  (setq git-gutter:modified-sign "~ ")
  (setq git-gutter:added-sign "+ ")
  (setq git-gutter:deleted-sign "- ")
  (setq git-gutter:unchanged-sign "  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit
;; newline after 72 chars in magit-log-edit-mode
(after 'magit
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
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
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
;;;; mercurial
(require 'ahg nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nrepl
(defun live-windows-hide-eol ()
 "Do not show ^M in files containing mixed UNIX and DOS line endings."
 (interactive)
 (setq buffer-display-table (make-display-table))
 (aset buffer-display-table ?\^M []))


(when (eq system-type 'windows-nt)
  (add-hook 'nrepl-mode-hook 'live-windows-hide-eol ))

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

(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

;;Auto Complete
;;(require 'ac-nrepl )
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; specify the print length to be 100 to stop infinite sequences killing things.
(defun live-nrepl-set-print-length ()
  (nrepl-send-string-sync "(set! *print-length* 100)" "clojure.core"))

(add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length)

;; Monkey Patch nREPL with better behaviour:

(defun live-nrepl-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
  ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
  (let ((replp (equal 'nrepl-mode (buffer-local-value 'major-mode buffer))))
    (if (or (and nrepl-popup-stacktraces-in-repl replp)
            (and nrepl-popup-stacktraces (not replp)))
        (lexical-let ((nrepl-popup-on-error nrepl-popup-on-error)
                      (err-buffer (nrepl-popup-buffer nrepl-error-buffer t)))
          (with-current-buffer buffer
            (nrepl-send-string "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                        (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
                               (nrepl-make-response-handler err-buffer
                                                            '()
                                                            (lambda (err-buffer str)
                                                              (with-current-buffer err-buffer (goto-char (point-max)))
                                                              (nrepl-emit-into-popup-buffer err-buffer str)
                                                              (with-current-buffer err-buffer (goto-char (point-min)))
                                                              )
                                                            (lambda (err-buffer str)
                                                              (with-current-buffer err-buffer (goto-char (point-max)))
                                                              (nrepl-emit-into-popup-buffer err-buffer str)
                                                              (with-current-buffer err-buffer (goto-char (point-min)))
                                                              )
                                                            '())
                               (nrepl-current-ns)
                               (nrepl-current-tooling-session)))
          (with-current-buffer nrepl-error-buffer
            (compilation-minor-mode 1))
          ))))

;;(setq nrepl-err-handler 'live-nrepl-err-handler)

;; Region discovery fix
(defun nrepl-region-for-expression-at-point ()
  "Return the start and end position of defun at point."
  (when (and (live-paredit-top-level-p)
             (save-excursion
               (ignore-errors (forward-char))
               (live-paredit-top-level-p)))
    (error "Not in a form"))

  (save-excursion
    (save-match-data
      (ignore-errors (live-paredit-forward-down))
      (paredit-forward-up)
      (while (ignore-errors (paredit-forward-up) t))
      (let ((end (point)))
        (backward-sexp)
        (list (point) end)))))

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
                       (nrepl-current-tooling-session))))

(setq nrepl-port "4555")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; yasnippet
(after 'yasnippet-autoloads
  (yas-global-mode 1)
  (after 'yasnippet
    (setq yas-keymap  (let ((map (make-sparse-keymap)))
                        (define-key map [(control tab)] 'yas-next-field-or-maybe-expand)
                        (define-key map (kbd "C-TAB")   'yas-next-field-or-maybe-expand)
                        (define-key map [(shift tab)]   'yas-prev-field)
                        (define-key map [backtab]       'yas-prev-field)
                        (define-key map (kbd "C-g")     'yas-abort-snippet)
                        (define-key map (kbd "C-d")     'yas-skip-and-clear-or-delete-char)
                        map))
    (require 'dropdown-list)
    (setq yas-prompt-functions '(yas-dropdown-prompt
                                 yas-ido-prompt
                                 yas-x-prompt
                                 yas-completing-prompt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; desktop.el
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; undo-tree
(after 'undo-tree-autoloads
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; browse-kill-ring
(after 'browse-kill-ring-autoloads
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-no-duplicates t)
  (setq browse-kill-ring-display-duplicates nil)
  (setq browse-kill-ring-highlight-inserted-item nil)
  (browse-kill-ring-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; insert-time.el
(with-library 'insert-time
  (setq insert-date-format "%Y-%m-%d")
  (setq insert-time-format "%H:%M:%S"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pomodoro.el
(after 'pomodoro-autoloads
  (autoload 'pomodoro-add-to-mode-line "pomodoro" t)
  (pomodoro-add-to-mode-line)
  (setq pomodoro-sound-player "/usr/bin/paplay")
  (setq pomodoro-break-start-sound (concat ptrv-etc-dir "sounds/alarm.wav"))
  (setq pomodoro-work-start-sound (concat ptrv-etc-dir "sounds/alarm.wav")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sql-mode
;; switch between sqlite3 and spatialite excutable
(defun sql-switch-spatialite-sqlite ()
  (interactive)
  (let* ((sqlprog sql-sqlite-program)
         (change (if (string-match "sqlite" sqlprog)
                     (executable-find "spatialite")
                   (executable-find "sqlite3"))))
    (setq sql-sqlite-program change)
    (message "sql-sqlite-program changed to %s" change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; the silver searcher
(setq ag-highlight-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pure-mode
(autoload 'pure-mode "pure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pure\\(rc\\)?$" . pure-mode))
(after 'pure-mode
  (add-hook 'pure-mode-hook 'hs-minor-mode)
  (add-hook 'pure-eval-mode-hook
            (lambda ()
              (define-key pure-eval-mode-map [up] 'comint-previous-input)
              (define-key pure-eval-mode-map [down] 'comint-next-input)))
  (define-key pure-mode-map (kbd "C-c M-p") 'run-pure)
  (define-key pure-mode-map (kbd "C-x M-p") 'pure-scratchpad))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smart-operator
(autoload 'smart-operator-mode "smart-operator" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tea-time
(autoload 'tea-time "tea-time" nil t)
(setq tea-time-sound (concat ptrv-etc-dir "sounds/alarm.wav"))
(cond (*is-mac*
       (setq tea-time-sound-command "afplay %s"))
      (*is-linux*
       (setq tea-time-sound-command "paplay %s")))

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
(after 'iflipb-autoloads
  (setq iflipb-ignore-buffers '("*Ack-and-a-half*"
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
                                "SCLang:PostBuffer*"
                                ))
  (setq iflipb-wrap-around t)
  (after 'iflipb-autoloads
    (global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
    (global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
    (global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
    (global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ack-and-a-half
(after 'ack-and-a-half-autoloads
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; edit-server
(after 'edit-server-autoloads
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
  (edit-server-start)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . gfm-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; iedit
(after 'iedit-autoloads
  (setq iedit-toggle-key-default (kbd "C-;"))
  (define-key global-map iedit-toggle-key-default 'iedit-mode)
  (define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
  (define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
  (define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smooth-scrolling
(after 'smooth-scrolling-autoloads
  (setq smooth-scroll-margin 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; google-this
(after 'google-this-autoloads
  (google-this-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; url
(setq url-configuration-directory (concat ptrv-tmp-dir "url"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; projectile
(after 'projectile-autoloads
  (setq projectile-known-projects-file (concat
                                        ptrv-tmp-dir
                                        "projectile-bookmarks.eld")
        projectile-cache-file (concat ptrv-tmp-dir "projectile.cache"))

  (projectile-global-mode)

  (after 'projectile
    (add-to-list 'projectile-project-root-files ".ropeproject" t)
    (add-to-list 'projectile-project-root-files "setup.py" t)
    (define-key projectile-mode-map (kbd "C-c p f") 'nil)
    (define-key projectile-mode-map (kbd "C-c p F") 'projectile-find-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ffip
(after 'find-file-in-project
  (setq ffip-project-file '(".git" ".hg" ".ropeproject" "setup.py")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; popwin
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd " C-z") popwin:keymap)

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
        ("*Compile-Log" :height 20 :stick t)
        ("*pytest*" :noselect t)
        ("*Python*" :stick t)
        ("*Python Doc*" :noselect t)
        ("*jedi:doc*" :noselect t)
        ("*Registers*" :noselect t)
        ("*ielm*" :stick t)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; buffer
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default indent-tabs-mode nil) ; And force use of spaces
(setq-default c-basic-offset 4)     ; indents 4 chars
(setq-default tab-width 4)          ; and 4 char wide for TAB

(custom-set-variables
 '(tab-stop-list (quote (2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/org/newgtd.org"))
(global-set-key (kbd "C-c g") 'gtd)

(setq org-replace-disputed-keys t)

(after 'org
  (message "Org config has been loaded !!!")
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-iswitchb nil)
  (setq org-completion-use-ido t)

  (setq org-log-done t)

  (setq org-clock-into-drawer t)
  ;;(setq org-clock-idle-time 10)
  (setq org-src-fontify-natively nil)
  ;; Override

  ;; yasnippet workaround
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (defun org-mode-yasnippet-workaround ()
    (make-variable-buffer-local 'yas/trigger-key)
    (setq yas/trigger-key [tab])
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas/keymap [tab] 'yas/next-field))

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (defun org-mode-init ()
    ;; (local-set-key [(control meta return)] 'org-insert-heading)
    ;; (local-set-key [(control shift left)] 'previous-buffer)
    ;; (local-set-key [(control shift right)] 'next-buffer)
    ;; (local-set-key [(meta shift right)] 'ido-switch-buffer)
    ;; (local-set-key [(meta shift left)] 'magit-status)
    (auto-complete-mode -1)
    (turn-off-flyspell)
    (org-mode-yasnippet-workaround))

  (add-hook 'org-mode-hook 'org-mode-init)

  (setq org-default-notes-file "~/Dropbox/org/captures.org")
  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/Dropbox/org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/MobileOrg")
  ;; Set files to push to org-mobile-directory
  (setq org-mobile-files (quote ("~/Dropbox/org/newgtd.org"
                                 "~/Dropbox/org/uni.org"
                                 "~/Dropbox/org/notes.org"
                                 "~/Dropbox/org/journal.org"
                                 "~/Dropbox/org/master_thesis.org")))
  ;; Set to the files (or directory of files) you want sync'd
  (setq org-agenda-files (quote ("~/Dropbox/org/newgtd.org"
                                 "~/Dropbox/org/uni.org"
                                 "~/Dropbox/org/master_thesis.org")))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")

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
                         (quote ((agenda time-up priority-down tag-up) )))
                        (org-deadline-warning-days 0)
                        ))))
          ))

  (setq org-ditaa-jar-path "~/applications/ditaa.jar")
  (setq org-plantuml-jar-path "~/applications/plantuml.jar")

  (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

                                        ; Make babel results blocks lowercase
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
     (gnuplot . t)
     ))

  ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
  (add-to-list 'org-src-lang-modes '("sam" . sam))

  ;; Open mailto links in gmail
  (setq org-link-mailto-program
        '(browse-url "https://mail.google.com/mail/?view=cm&to=%a&su=%s")))

(after 'calendar
  (setq calendar-week-start-day 1))

;; org publish projects file
(after 'ox
  (load "~/.org-publish-projects.el" 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org2blog
(require 'org2blog-autoloads)
(after 'org2blog
  (load "~/.org-blogs.el" 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; org-present
(autoload 'org-present "org-present" nil t)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(after 'latex
  (message "latex config has been loaded !!!")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  (setq TeX-source-correlate-method 'synctex)

  (after 'tex
    (add-to-list 'TeX-command-list
                  '("latexmk"
                    "latexmk %s"
                    TeX-run-TeX nil (latex-mode doctex-mode) :help "Run latexmk") t)
    (add-to-list 'TeX-command-list
                 '("latexmk clean"
                   "latexmk -c %s"
                   TeX-run-TeX nil (latex-mode doctex-mode) :help "Run latexmk -c") t)
    (add-to-list 'TeX-command-list
                 '("latexmk cleanall"
                   "latexmk -C %s"
                   TeX-run-TeX nil (latex-mode doctex-mode) :help "Run latexmk -C") t))

  (defun okular-make-url () (concat
                             "file://"
                             (expand-file-name (funcall file "pdf" t)
                                               (file-name-directory (TeX-master-file)))
                             "#src:"
                             (TeX-current-line) (buffer-file-name)))

  (cond (*is-linux*
         (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
         (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (add-to-list 'TeX-expand-list
                                  '("%u" okular-make-url)))))
        (*is-mac*
         (setq TeX-view-program-selection '((output-pdf "Skim")))
         (setq TeX-view-program-list
               '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))))


  (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)

  (custom-set-variables
   '(reftex-ref-style-default-list (quote ("Hyperref")))
   ;; '(reftex-cite-format 'natbib)
   )

  (autoload 'info-lookup-add-help "info-look" nil nil)
  (info-lookup-add-help
   :mode 'latex-mode
   :regexp ".*"
   :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
   :doc-spec '(("(latex2e)Concept Index" )
               ("(latex2e)Command Index")))

  (require 'auto-complete-auctex)
  (define-key LaTeX-mode-map (kbd "C-c ä") 'LaTeX-close-environment)
  (define-key LaTeX-mode-map (kbd "C-c ü") 'TeX-next-error)
  (require 'pstricks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; filetypes
(add-to-list 'auto-mode-alist '("\\.zsh-template$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;;(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("lib/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("etc/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; pd-mode
(autoload 'pd-mode "pd-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.pat$" . pd-mode))
(add-to-list 'auto-mode-alist '("\\.pd$"  . pd-mode))

;; gitconfig
(add-to-list 'auto-mode-alist '("gitconfig*" . gitconfig-mode))

;; cmake
(autoload 'cmake-mode "cmake-mode" "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; desktop-entry-mode
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(add-to-list 'auto-mode-alist
             '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))
(add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)

;; glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; IanniX
(add-to-list 'auto-mode-alist '("\\.nxscript$" . js-mode))

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; ChucK
(autoload 'chuck-mode "chuck-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ck$" . chuck-mode))

;; arduino
(autoload 'arduino-mode "arduino-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

;; arch linux
(add-to-list 'auto-mode-alist '("PKGBUILD$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . shell-script-mode))

;;;; abbrev
(define-abbrev-table 'global-abbrev-table
  '(
    ;; typo corrections
    ("teh" "the")
    ))
;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pandoc
(autoload 'turn-on-pandoc "pandoc-mode" nil t)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-to-list 'auto-mode-alist '("\\.text$" .  markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; golang
(exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "GOPATH")

(after 'go-mode
  (message "go-mode config has been loaded !!!")

  ;; go-lang completion
  (add-to-list 'load-path (concat
                           (car (split-string (getenv "GOPATH") ":"))
                           "/src/github.com/nsf/gocode/emacs"))

  (require 'go-autocomplete)
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
          'ac-go-mode-selection-face)

  ;; (defun go-dot-complete ()
  ;;   "Insert dot and complete code at point."
  ;;   (interactive)
  ;;   (insert ".")
  ;;   (unless (ac-cursor-on-diable-face-p)
  ;;     (auto-complete '(ac-source-go))))

  ;; compile fucntions
  (defun go-cmd-build ()
    "compile project"
    (interactive)
    (compile "go build"))

  (defun go-cmd-test ()
    "test project"
    (interactive)
    (compile "go test -v"))

  (defun go-chk ()
    "gocheck project"
    (interactive)
    (compile "go test -gocheck.vv"))

  (defun go-run ()
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

  (defun go-run-buffer ()
    "go run current buffer"
    (interactive)
    (compile (concat "go run " buffer-file-name)))

  (defun go-mode-init ()
    (make-local-variable 'before-save-hook)
    (setq before-save-hook 'gofmt-before-save)
    (hs-minor-mode 1)
    ;;(flycheck-mode-on-safe)
    (local-set-key (kbd "M-.") 'godef-jump)
    (define-key go-mode-map (kbd "C-c C-c c") 'go-run)
    (define-key go-mode-map (kbd "C-c C-c r") 'go-run-buffer)
    (define-key go-mode-map (kbd "C-c C-c b") 'go-cmd-build)
    (define-key go-mode-map (kbd "C-c C-c t") 'go-cmd-test)
    (define-key go-mode-map (kbd "C-c C-c g") 'go-chk)
    (define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
    (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
    (define-key go-mode-map (kbd "C-c C-p") 'go-create-package)
    (define-key go-mode-map "." 'ac-dot-complete))

  (add-hook 'go-mode-hook 'go-mode-init)

  ;; flycheck support
  (add-to-list 'load-path (concat
                           (car (split-string (getenv "GOPATH") ":"))
                           "/src/github.com/dougm/goflymake"))
  ;; (add-to-list 'load-path (concat
  ;;                          (car (split-string (getenv "GOPATH") ":"))
  ;;                          "/src/github.com/ptrv/goflycheck"))
  (require 'go-flycheck)

  (after 'flycheck
    (flycheck-declare-checker go
      "A Go syntax and style checker using the gofmt utility. "
      :command '("gofmt" source)
      :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
      :modes 'go-mode
      :next-checkers '((no-errors . go-goflymake))
      )
    (add-to-list 'flycheck-checkers 'go t)
       ;; remove go-goflymake from begin of list and add it to the end

    (setq flycheck-checkers (remove 'go-goflymake flycheck-checkers))
    (add-to-list 'flycheck-checkers 'go-goflymake t)))

(defun go-create-package (name &optional arg)
  "Create a new sketch with NAME under GOPATH src folder.

If ARG is not nil, create package in current directory"
  (interactive "sInsert new package name: \nP")
  (let ((name (remove ?\s name))
        (root-dir (concat (car (split-string (getenv "GOPATH") ":"))
                          "/src/github.com/ptrv")))
    (if (not (string-equal "" name))
        (progn
          (unless arg (setq name (concat root-dir "/" name)))
          (make-directory name)
          (find-file (concat name "/main.go")))
      (error "Please insert a package name"))))

(when (executable-find "errcheck")
  (autoload 'go-errcheck "go-errcheck" nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xml
(add-to-list 'auto-mode-alist '("\\.gpx$" . nxml-mode))

(after 'nxml-mode
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
  ;; typing a slash automatically completes the end-tag
  (setq nxml-slash-auto-complete-flag t)
  ;; treat an element as a single expression instead of only tag
  (setq nxml-sexp-element-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; erc
(when (and (boundp 'freenode-user)
           (boundp 'freenode-pass))

  (defun erc-connect ()
    "Start up erc and connect to freedonde"
    (interactive)
    (erc :server "irc.freenode.net"
         :full-name "Peter V."
         :port 6667
         :nick freenode-user
         ))
  (after 'erc
    (erc-services-mode 1)
    (setq erc-prompt-for-nickserv-password nil)
    (setq erc-nickserv-passwords
          `((freenode ((,freenode-user . ,freenode-pass)))))

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

(after 'erc
  ;;change wrap width when window is resized
  (make-variable-buffer-local 'erc-fill-column)
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
(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Synth-A-Modeler mode
(setq auto-mode-alist (cons '("\\.mdl$" . sam-mode) auto-mode-alist))
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

;; Subword mode (consider CamelCase chunks as words)
;;(global-subword-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; move-text
(move-text-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

;; (setq-default markdown-command
;;               (concat
;;                "pandoc -S -s --self-contained -f markdown -t html5 --css="
;;                markdown-css-path
;;                " "))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(setq markdown-css-path (expand-file-name
                         (concat
                          (file-name-directory
                           (or load-file-name (buffer-file-name)))
                          "../etc/css/markdown.css")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; processing
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(autoload 'processing-snippets-initialize "processing-mode" nil nil nil)
;;(eval-after-load 'yasnippet '(processing-snippets-initialize))
(autoload 'processing-find-sketch "processing-mode" nil t)

(after 'processing-mode
  (message "Processing config has been loaded !!!")

  (after 'yasnippet
    (processing-snippets-initialize))

  (cond (*is-mac*
         (setq processing-location "processing-java"))
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
    (setq ac-user-dictionary processing-functions)
    (setq ac-user-dictionary (append ac-user-dictionary processing-builtins))
    (setq ac-user-dictionary (append ac-user-dictionary processing-constants))
    )

  (add-to-list 'ac-modes 'processing-mode)
  (add-hook 'processing-mode-hook 'processing-mode-init)

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
(after 'flycheck-autoloads
  (unless (and (>= emacs-major-version 24)
               (>= emacs-minor-version 3))
    (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
    (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
    (add-to-list 'debug-ignored-errors "\\`Configured syntax checker .* cannot be used\\'"))

  (add-hook 'after-init-hook #'global-flycheck-mode)

  (add-hook 'emacs-lisp-mode-hook #'(lambda () (flycheck-mode -1)))
  (add-hook 'lisp-interaction-mode #'(lambda () (flycheck-mode -1)))

  (after 'flycheck
    (setq flycheck-highlighting-mode 'lines)))

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
(after 'autopair-autoloads
  (autopair-global-mode 1)

  (defadvice enable-paredit-mode (before disable-autopair activate)
    (setq autopair-dont-activate t)
    (autopair-mode -1))

  (add-hook 'python-mode-hook
            #'(lambda ()
                (setq autopair-handle-action-fns
                      (list #'autopair-default-handle-action
                            #'autopair-python-triple-quote-action))))

  ;; (add-hook 'c++-mode-hook
  ;;           #'(lambda ()
  ;;               (push '(?< . ?>)
  ;;                     (getf autopair-extra-pairs :code))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clean-mode-line
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (paredit-mode . " Φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " τ")
    (elisp-slime-nav-mode . " δ")
    (nrepl-interaction-mode . " ηζ")
    (auto-fill-function . " φ")
    (autopair-mode . "")
    (projectile-mode . "")
    (kibit-mode . " κ")
    (google-this-mode . "")
    ;; Major modes
    (nrepl-mode . "ηζ")
    (clojure-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (markdown-mode . "md")
    (processing-mode . "P5"))
  "Alist for `clean-mode-line'.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; osx
(when *is-mac*
 (setq mac-option-key-is-meta nil)
 (setq mac-command-key-is-meta t)
 (setq mac-command-modifier 'meta)
 (setq mac-option-modifier nil)

 (if (window-system)
     (progn
       (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
       (set-frame-size (selected-frame) 120 52)
       (set-frame-position (selected-frame) 100 24)))

 (setq default-input-method "MacOSX")

 ;; Make cut and paste work with the OS X clipboard

 (defun live-copy-from-osx ()
   (shell-command-to-string "pbpaste"))

 (defun live-paste-to-osx (text &optional push)
   (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

 (when (not window-system)
   (setq interprogram-cut-function 'live-paste-to-osx)
   (setq interprogram-paste-function 'live-copy-from-osx))

 ;; Work around a bug on OS X where system-name is a fully qualified
 ;; domain name
 (setq system-name (car (split-string system-name "\\.")))

 ;; Ignore .DS_Store files with ido mode
 (add-to-list 'ido-ignore-files "\\.DS_Store"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; linux
(when *is-linux*
  (set-frame-font "Inconsolata-12" nil t)
  (autoload 'pcomplete/apt-get "pcmpl-apt" nil nil)

  (defun setup-frame-hook (frame)
    ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
    ;;(run-with-idle-timer 0.2 nil 'toggle-fullscreen)
    )
  (add-hook 'after-make-frame-functions 'setup-frame-hook)

  ;; erc notification
  (after 'erc
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
  (setq typewriter-play-command "paplay %s")
  (setq typewriter-sound-default (concat
                                  ptrv-etc-dir
                                  "sounds/9744__horn__typewriter.wav"))
  (setq typewriter-sound-end (concat
                              ptrv-etc-dir
                              "sounds/eol-bell.wav"))
  (setq typewriter-sound-return (concat
                                 ptrv-etc-dir
                                 "sounds/carriage-return.wav")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; superollider
(after 'w3m
  (define-key w3m-mode-map [left] 'backward-char)
  (define-key w3m-mode-map [right] 'forward-char)
  (define-key w3m-mode-map [up] 'previous-line)
  (define-key w3m-mode-map [down] 'next-line)
  (setq w3m-auto-show 1)
  (setq w3m-key-binding 'info)
  (setq w3m-pop-up-frames t)
  (setq w3m-pop-up-windows nil))

(defun sclang-ptrv ()
  (interactive)
  (if (require 'sclang nil t)
      (sclang-start)
    (message "SCLang is not installed!")))

(after 'sclang
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
      (local-set-key [backtab] 'backward-button))

    )
  (add-hook 'sclang-mode-hook 'supercollider-init)

  (after 'sclang
    (define-key sclang-mode-map (kbd "C-c ö") 'sclang-dump-interface)
    (define-key sclang-mode-map (kbd "C-c ü") 'sclang-dump-full-interface)
    (define-key sclang-mode-map (kbd "C-c ä") 'sclang-pop-definition-mark)
    ;; Raise all supercollider windows.
    (define-key sclang-mode-map (kbd "C-c f")
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
        (sclang-eval-string "HelperWindow.new;"))))

  (if (fboundp 'completing-read-ido)
      (progn
        (add-to-list 'ido-ubiquitous-command-exceptions 'sclang-dump-interface)
        (add-to-list 'ido-ubiquitous-command-exceptions 'sclang-dump-full-interface))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elpy
(after 'python
  (after 'elpy-autoloads
    (setq elpy-default-minor-modes '(eldoc-mode
                                     highlight-indentation-mode
                                     yas-minor-mode
                                     auto-complete-mode))
    (elpy-enable t)

    (add-hook 'elpy-mode-hook #'(lambda ()
                                  (when (file-remote-p (buffer-file-name))
                                    (elpy-disable))))))

(after 'elpy
  (define-key elpy-mode-map (kbd "C-c C-f") nil)
  (define-key elpy-mode-map (kbd "C-c C-j") nil)
  (define-key elpy-mode-map (kbd "C-c C-n") nil)
  (define-key elpy-mode-map (kbd "C-c C-p") nil)
  (setq python-check-command "flake8")
  (add-hook 'python-mode-hook 'elpy-initialize-local-variables)
  ;; complete on dot
  (define-key elpy-mode-map "." 'ac-dot-complete)

  (defun elpy-use-ipython-pylab ()
    "Set defaults to use IPython instead of the standard interpreter."
    (interactive)
    (unless (boundp 'python-python-command)
      (elpy-use-ipython)
      (setq python-shell-interpreter-args "--pylab")))
  )

(after 'highlight-indentation
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; python
(after 'python
  (exec-path-from-shell-copy-env "PYTHONPATH")

  ;; pytest
  (autoload 'pytest-all "pytest" nil t)
  (autoload 'pytest-module "pytest" nil t)
  (autoload 'pytest-one "pytest" nil t)
  (autoload 'pytest-directory "pytest" nil t)
  (setq pytest-global-name "py.test")
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-a") 'pytest-all)
              (local-set-key (kbd "C-c C-,") 'pytest-module)
              (local-set-key (kbd "C-c C-.") 'pytest-one)
              ;;(local-set-key (kbd "C-c C-d") 'pytest-directory)
              ;; (local-set-key (kbd "C-c t p a") 'pytest-pdb-all)
              ;; (local-set-key (kbd "C-c t p m") 'pytest-pdb-module)
              ;; (local-set-key (kbd "C-c t p .") 'pytest-pdb-one)
              ))
  ;; info
  (autoload 'info-lookup-add-help "info-look" nil nil)
  (info-lookup-add-help
   :mode 'python-mode
   :regexp "[[:alnum:]_]+"
   :doc-spec
   '(("(python)Index" nil "")))

  ;; PythonTidy
  (defun pytidy-whole-buffer ()
    (interactive)
    (let ((a (point)))
      (shell-command-on-region (point-min) (point-max) "pythontidy" t)
      (goto-char a)))

  ;; pylookup
  (eval-when-compile (require 'pylookup))
  (setq pylookup-dir (concat dotfiles-dir "site-lisp/pylookup"))
  (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
  (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

  ;; set search option if you want
  ;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

  ;; to speedup, just load it on demand
  (autoload 'pylookup-lookup "pylookup"
    "Lookup SEARCH-TERM in the Python HTML indexes." t)

  (autoload 'pylookup-update "pylookup"
    "Run pylookup-update and create the database at `pylookup-db-file'." t)

  (define-key python-mode-map (kbd "C-c L") 'pylookup-lookup)

  ;; pylint
  (autoload 'pylint "pylint")
  (add-hook 'python-mode-hook 'pylint-add-menu-items)
  (add-hook 'python-mode-hook 'pylint-add-key-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cc-mode
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; Hook auto-complete into clang
(after 'cc-mode
  (message "cc-mode config has been loaded !!!")
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


  (add-hook 'c++-mode-hook
            (lambda ()
              (unless (string-match ".*flycheck.*" buffer-file-name)
                (setq ac-sources '(ac-source-clang-async))
                (ac-clang-launch-completion-process))
              ;; (dtrt-indent-mode 1)
              (set (make-local-variable 'before-save-hook) nil)))

  (defun set-ff-find-other-file-binding ()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file))
  (add-hook 'c-mode-hook 'set-ff-find-other-file-binding)
  (add-hook 'c++-mode-hook 'set-ff-find-other-file-binding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; x11
(when (eq window-system 'x)
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
(after 'multiple-cursors-autoloads
  ;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-ö") 'mc/edit-lines)
  (global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-ö") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-ä") 'mc/mark-all-like-this))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; expand-region
(after 'expand-region-autoloads
  (global-set-key (kbd "C-ü") 'er/expand-region)
  (global-set-key (kbd "C-Ü") 'er/contract-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key-chord
(after 'key-chord-autoloads
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'switch-to-previous-buffer)
  (key-chord-define-global "KK" 'winner-undo)
  (key-chord-define-global "LL" 'winner-redo)
  (key-chord-define-global "BB" 'ido-switch-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bindings
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x f") 'ido-recentf-open)
(global-set-key (kbd "C-c p f") 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'idomenu)

(global-set-key [f5] 'refresh-file)

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'delete-other-windows)

(global-set-key (kbd "C-x g") 'magit-status)

;;mark current function
(global-set-key (kbd "C-x C-p") 'mark-defun)

;;emacs-lisp shortcuts
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-c m s") 'eval-and-replace) ;swap
(global-set-key (kbd "C-c m b") 'eval-buffer)
(global-set-key (kbd "C-c m e") 'eval-last-sexp)
(global-set-key (kbd "C-c m i") 'eval-expression)
(global-set-key (kbd "C-c m d") 'eval-defun)
(global-set-key (kbd "C-c m n") 'eval-print-last-sexp)
(global-set-key (kbd "C-c m r") 'eval-region)

;;diff shortcuts
(global-set-key (kbd "C-c d f") 'diff-buffer-with-file)

(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)

(global-set-key (kbd "C-c w .") (lambda () (interactive) (shrink-window-horizontally 4)))
(global-set-key (kbd "C-c w ,") (lambda () (interactive) (enlarge-window-horizontally 4)))
(global-set-key (kbd "C-c w <down>") (lambda () (interactive) (enlarge-window -4)))
(global-set-key (kbd "C-c w <up>") (lambda () (interactive) (enlarge-window 4)))

;; winner undo and redo
(global-set-key (kbd "C-c w b") 'winner-undo)
(global-set-key (kbd "C-c w f") 'winner-redo)

;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

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

;; Ace jump mode
(after 'ace-jump-mode-autoloads
  (global-set-key (kbd "C-o") 'ace-jump-mode))

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'ptrv-lisp-describe-thing-at-point)
(after 'nrepl
  (define-key nrepl-mode-map (kbd "M-RET") 'nrepl-doc)
  (define-key nrepl-interaction-mode-map (kbd "M-RET") 'nrepl-doc))

;; Make Emacs use "newline-and-indent" when you hit the Enter key so
;; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m" 'newline-and-indent)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(define-key ac-completing-map "\r" 'ac-complete)

;; kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'comment-dwim-line)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region-below)

(global-set-key (kbd "C-S-M-d") 'duplicate-line-below-comment)

(global-set-key (kbd "C-c q") 'exit-emacs-client)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "<C-M-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(after 'sql
  (define-key sql-mode-map (kbd "C-c C-p p") 'sql-set-product)
  (define-key sql-mode-map (kbd "C-c C-p i") 'sql-set-sqli-buffer)
  (define-key sql-mode-map (kbd "C-c C-p s") 'sql-switch-spatialite-sqlite))

;; http://irreal.org/blog/?p=1742
(global-set-key (kbd "C-c t")
                #'(lambda ()
                    "Bring up a full-screen eshell or restore previous config."
                    (interactive)
                    (if (string= "eshell-mode" major-mode)
                        (jump-to-register :eshell-fullscreen)
                      (progn
                        (window-configuration-to-register :eshell-fullscreen)
                        (eshell)
                        (delete-other-windows)))))

(global-set-key (kbd "M-j") (lambda ()
                              (interactive)
                              (join-line -1)))

;; (global-set-key (kbd "<f11>") 'toggle-fold)
(global-set-key (kbd "<f11>") 'hs-toggle-hiding)
(global-set-key (kbd "S-<f11>") 'toggle-fold-all)

;; autopair-newline interferes with cua-rotate-rectangle (default binding "\r")
(after 'cua-rect
  (define-key cua--rectangle-keymap (kbd "M-<return>") 'cua-rotate-rectangle))

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
(global-set-key (kbd "C-x r v") 'ptrv-list-registers)

;; (set-register ?e '(file . "~/.emacs.d"))
;; (set-register ?i '(file . "~/.emacs.d/init.el"))
;; (set-register ?m '(file . "~/.emacs.d/modules"))
(set-register ?z '(file . "~/.oh-my-zsh"))
(set-register ?o '(file . "~/Dropbox/org/newgtd.org"))

(define-key Info-mode-map "ä" 'Info-forward-node)
(define-key Info-mode-map "ö" 'Info-backward-node)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'visit-ielm)

(global-set-key (kbd "<C-f9>") #'global-git-gutter-mode)

(global-set-key (kbd "C-c I") 'find-user-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defuns
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t nil))

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

;; Makes load time faster.
(defun byte-recompile-home ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

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
  (kill-emacs)
  )

(defun exit-emacs-client ()
  "consistent exit emacsclient.
   if not in emacs client, echo a message in minibuffer, don't exit emacs.
   if in server mode
      and editing file, do C-x # server-edit
      else do C-x 5 0 delete-frame"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (delete-frame)))

;; http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
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
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-char (point-min))
        (forward-line (1- (read-number "Goto line: "))))
    (linum-mode -1)))

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

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de_DE-neu") "american" "de_DE-neu")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

;; http://stackoverflow.com/a/4988206
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))
(global-set-key (kbd "C-c v") 'halve-other-window-height)

(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)))

(defun ergoemacs-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   (*is-mac* (shell-command "open ."))
   (*is-linux*
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if *is-mac*
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

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

;; http://emacsredux.com/blog/2013/03/28/google/
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

;; from emacs-live
(require 'uuid)
(defun ptrv-persistent-scratch-buffer ()
  "Create a new persistent empty buffer (i.e. saved as a file)"
  (interactive)
  (let* ((id (uuid-string))
         (fname (concat ptrv-pscratch-dir id))
         (buf (get-buffer-create id)))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (save-buffer)
      (auto-save-mode 1))))

(defun ptrv-user-first-name ()
  (let* ((first-name (car (split-string user-full-name))))
    (if first-name
        (capitalize first-name)
      "")))

(defun ptrv-user-first-name-p ()
  (not (string-equal "" (ptrv-user-first-name))))

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
(defun ptrv-list-registers ()
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

;; http://emacsredux.com/blog/2013/04/29/start-command-or-switch-to-its-buffer/
(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (start-or-switch-to 'ielm "*ielm*"))

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun ptrv-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; custom settings
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; welcome-message stuff
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
