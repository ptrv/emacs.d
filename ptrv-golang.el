;;; ptrv-golang.el --- golang-conf

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang-conf.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "GOPATH")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-lang completion
(add-to-list 'load-path (concat
                         (car (split-string (getenv "GOPATH") ":"))
                         "/src/github.com/nsf/gocode/emacs"))

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (defface ac-go-mode-candidate-face
       '((t (:background "lightgray" :foreground "navy")))
       "Face for go-autocomplete candidate"
       :group 'auto-complete)
     (defface ac-go-mode-selection-face
       '((t (:background "navy" :foreground "white")))
       "Face for the go-autocomplete selected candidate."
       :group 'auto-complete)
     (setcar (nthcdr 1 ac-source-go)
             '(candidate-face . ac-go-mode-candidate-face))
     (setcar (nthcdr 2 ac-source-go)
             '(selection-face . ac-go-mode-selection-face))))

(defun go-dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (unless (ac-cursor-on-diable-face-p)
    (auto-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
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
  (define-key go-mode-map "." 'go-dot-complete))

(add-hook 'go-mode-hook 'go-mode-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck support
;; (add-to-list 'load-path (concat
;;                          (car (split-string (getenv "GOPATH") ":"))
;;                          "/src/github.com/ptrv/goflymake"))
;; (require 'go-flycheck)

;; (eval-after-load 'flycheck
;;   '(progn
;;      (flycheck-declare-checker go
;;        "A Go syntax and style checker using the gofmt utility. "
;;        :command '("gofmt" source)
;;        :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
;;        :modes 'go-mode
;;        :next-checkers '((no-errors . go-goflymake)))
;;      (add-to-list 'flycheck-checkers 'go)))

(require 'flycheck-go-alt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Fix *Gofmt errors* window when using popwin
;; (eval-after-load 'go-mode
;;   '(progn
;;      (defadvice gofmt--process-errors (around gofmt--process-errors-new
;;                                               (filename tmpfile errbuf)
;;                                               activate)
;;        (with-current-buffer errbuf
;;          (goto-char (point-min))
;;          (insert "gofmt errors:\n")
;;          (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
;;            (replace-match (file-name-nondirectory filename) t t nil 1))
;;          (compilation-mode)
;;          (display-buffer errbuf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-golang)
;;; ptrv-golang.el ends here
