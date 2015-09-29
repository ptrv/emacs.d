;;; ptrv-go.el --- ptrv's go-lang config             -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
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

(require 'go-mode)


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
      (error "Please insert a package name"))))

(provide 'ptrv-go)
;;; ptrv-go.el ends here
