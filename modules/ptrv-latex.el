;;; ptrv-latex.el --- latex-conf

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

;; Latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(eval-after-load "tex"
  '(progn
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
                    TeX-run-TeX nil (latex-mode doctex-mode) :help "Run latexmk -C") t)))

(defun okular-make-url () (concat
                           "file://"
                           (expand-file-name (funcall file "pdf" t)
                                             (file-name-directory (TeX-master-file)))
                           "#src:"
                           (TeX-current-line) (buffer-file-name)))

(cond ((eq system-type 'gnu/linux)

       ;; ;; SyncTeX basics
       ;; ;; un-urlify and urlify-escape-only should be improved to handle all special characters, not only spaces.
       ;; ;; The fix for spaces is based on the first comment on http://emacswiki.org/emacs/AUCTeX#toc20

       ;; (defun un-urlify (fname-or-url)
       ;;   "Transform file:///absolute/path from Gnome into /absolute/path with very limited support for special characters"
       ;;   (if (string= (substring fname-or-url 0 8) "file:///")
       ;;       (url-unhex-string (substring fname-or-url 7))
       ;;     fname-or-url))

       ;; (defun urlify-escape-only (path)
       ;;   "Handle special characters for urlify"
       ;;   (replace-regexp-in-string " " "%20" path))

       ;; (defun urlify (absolute-path)
       ;;   "Transform /absolute/path to file:///absolute/path for Gnome with very limited support for special characters"
       ;;   (if (string= (substring absolute-path 0 1) "/")
       ;;       (concat "file://" (urlify-escape-only absolute-path))
       ;;     absolute-path))

       ;; ;; SyncTeX backward search - based on http://emacswiki.org/emacs/AUCTeX#toc20, reproduced on http://tex.stackexchange.com/a/49840/21017

       ;; (defun th-evince-sync (file linecol &rest ignored)
       ;;   (let* ((fname (un-urlify file))
       ;;          (buf (find-file fname))
       ;;          (line (car linecol))
       ;;          (col (cadr linecol)))
       ;;     (if (null buf)
       ;;         (message "[Synctex]: Could not open %s" fname)
       ;;       (switch-to-buffer buf)
       ;;       (goto-line (car linecol))
       ;;       (unless (= col -1)
       ;;         (move-to-column col)))))

       ;; (defvar *dbus-evince-signal* nil)

       ;; (defun enable-evince-sync ()
       ;;   (require 'dbus)
       ;;   ;; cl is required for setf, taken from: http://lists.gnu.org/archive/html/emacs-orgmode/2009-11/msg01049.html
       ;;   (require 'cl)
       ;;   (when (and
       ;;          (eq window-system 'x)
       ;;          (fboundp 'dbus-register-signal))
       ;;     (unless *dbus-evince-signal*
       ;;       (setf *dbus-evince-signal*
       ;;             (dbus-register-signal
       ;;              :session nil "/org/gnome/evince/Window/0"
       ;;              "org.gnome.evince.Window" "SyncSource"
       ;;              'th-evince-sync)))))

       ;; (add-hook 'LaTeX-mode-hook 'enable-evince-sync)


       ;; ;; SyncTeX forward search - based on http://tex.stackexchange.com/a/46157

       ;; ;; universal time, need by evince
       ;; (defun utime ()
       ;;   (let ((high (nth 0 (current-time)))
       ;;         (low (nth 1 (current-time))))
       ;;     (+ (* high (lsh 1 16) ) low)))

       ;; ;; Forward search.
       ;; ;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz
       ;; (defun auctex-evince-forward-sync (pdffile texfile line)
       ;;   (let ((dbus-name
       ;;          (dbus-call-method :session
       ;;                            "org.gnome.evince.Daemon"  ; service
       ;;                            "/org/gnome/evince/Daemon" ; path
       ;;                            "org.gnome.evince.Daemon"  ; interface
       ;;                            "FindDocument"
       ;;                            (urlify pdffile)
       ;;                            t     ; Open a new window if the file is not opened.
       ;;                            )))
       ;;     (dbus-call-method :session
       ;;                       dbus-name
       ;;                       "/org/gnome/evince/Window/0"
       ;;                       "org.gnome.evince.Window"
       ;;                       "SyncView"
       ;;                       (urlify-escape-only texfile)
       ;;                       (list :struct :int32 line :int32 1)
       ;;                       (utime))))

       ;; (defun auctex-evince-view ()
       ;;   (let ((pdf (file-truename (concat default-directory
       ;;                                     (TeX-master-file (TeX-output-extension)))))
       ;;         (tex (buffer-file-name))
       ;;         (line (line-number-at-pos)))
       ;;     (auctex-evince-forward-sync pdf tex line)))

       ;; ;; New view entry: Evince via D-bus.
       ;; (setq TeX-view-program-list '())
       ;; (add-to-list 'TeX-view-program-list
       ;;              '("EvinceDbus" auctex-evince-view))

       ;; ;; Prepend Evince via D-bus to program selection list
       ;; ;; overriding other settings for PDF viewing.
       ;; (setq TeX-view-program-selection '())
       ;; (add-to-list 'TeX-view-program-selection
       ;;              '(output-pdf "EvinceDbus"))
       (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
       (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
       (add-hook 'LaTeX-mode-hook
                 (lambda ()
                   (add-to-list 'TeX-expand-list
                                '("%u" okular-make-url))))
       )
      ((eq system-type 'darwin)
       (setq TeX-view-program-selection '((output-pdf "Skim")))
       (setq TeX-view-program-list
             '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
       ))


(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; (add-hook 'LaTeX-mode-hook 'flymake-mode)
;; (defun flymake-get-tex-args (file-name)
;;   (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))


(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                              (interactive)
;;                              (wrap-region-remove-wrapper "`")))

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

(eval-after-load "latex"
  '(progn
     (require 'auto-complete-auctex)
     (define-key LaTeX-mode-map (kbd "C-c ä") 'LaTeX-close-environment)
     (define-key LaTeX-mode-map (kbd "C-c ü") 'TeX-next-error)
     (require 'pstricks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-latex)
;;; ptrv-latex.el ends here
