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
         (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
         (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
         (add-hook 'LaTeX-mode-hook
                   (lambda ()
                     (add-to-list 'TeX-expand-list
                                  '("%u" okular-make-url)))))
        ((eq system-type 'darwin)
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

  (eval-after-load "latex"
    '(progn
       (require 'auto-complete-auctex)
       (define-key LaTeX-mode-map (kbd "C-c ä") 'LaTeX-close-environment)
       (define-key LaTeX-mode-map (kbd "C-c ü") 'TeX-next-error)
       (require 'pstricks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-latex)
;;; ptrv-latex.el ends here
