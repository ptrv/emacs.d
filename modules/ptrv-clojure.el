;;; ptrv-clojure.el --- clojure-conf

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'clojure-mode
  '(progn
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
                                  nil)))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

;;(require 'clojure-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

;;command to align let statements
;;To use: M-x align-cljlet
;; (live-add-pack-lib "align-cljlet")
;; (require 'align-cljlet)

;;Treat hyphens as a word character when transposing words
(eval-after-load 'clojure-mode
  '(progn
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

))


(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'kibit-mode "kibit-mode" nil t)
(add-hook 'clojure-mode-hook 'kibit-mode)

(eval-after-load "kibit-mode"
'(progn
   (define-key kibit-mode-keymap (kbd "C-c C-n") 'nil)
   (define-key kibit-mode-keymap (kbd "C-c k c") 'kibit-check)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (flycheck-declare-checker clojure-kibit
;;   "A Clojure code analyzer using the kibit utility."
;;   :command `(,(concat kibit-mode-path "bin/kibit-flymake.sh") source)
;;   :error-patterns '(("\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:ERROR: .* CORRECTION: .*\\)" error))
;;   :modes 'clojure-mode)

;; (add-to-list 'flycheck-checkers 'clojure-kibit)

(add-hook 'clojure-mode-hook (lambda () (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push-mark when switching to nrepl via C-c C-z
(defadvice nrepl-switch-to-repl-buffer (around
                                        nrepl-switch-to-repl-buffer-with-mark
                                        activate)
  (with-current-buffer (current-buffer)
    (push-mark)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4clojure
(autoload '4clojure-problem "four-clj" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-clojure)
;;; ptrv-clojure.el ends here
