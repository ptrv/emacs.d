;;; company-ycmd-ext.el --- Yasnippet template expansion for company-ycmd  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords: tools

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

(require 's)
(require 'irony-snippet)
(require 'company-ycmd)

(defun company-ycmd-ext--post-complete-yas-snippet (str placeholders)
  (let ((ph-count 0)
        (from 0)
        to snippet)
    (while
        (setq to (car placeholders)
              snippet (concat
                       snippet
                       (substring str from to)
                       (format "${%d:%s}"
                               (cl-incf ph-count)
                               (s-trim (substring str
                                                  (car placeholders)
                                                  (cadr placeholders)))))
              from (cadr placeholders)
              placeholders (cddr placeholders)))
    ;; handle the remaining non-snippet string, if any.
    (concat snippet (substring str from) "$0")))

(defun company-ycmd-ext--get-placeholders (params)
  (let ((from 0)
        (to 0)
        placeholders)
    (while (string-match "\\([^,]+\\),?" params from)
      (setq from (match-beginning 0))
      (setq to (match-end 0))
      (setq placeholders (append placeholders `(,(1+ from) ,(1- to))))
      (setq from to)
      ;; (message "%s" (match-string 0 test-string))
      )
    ;; (message "%s" placeholders)
    placeholders))

(defun company-ycmd-ext-templatify (params)
  (let ((placeholders (company-ycmd-ext--get-placeholders params)))
    (if (and placeholders (irony-snippet-available-p))
        (irony-snippet-expand
         (company-ycmd-ext--post-complete-yas-snippet params placeholders))
      ;; (insert (substring str 0 (car placeholders)))
      )))

(defun ptrv/company-ycmd--post-completion (arg)
  (let ((params (company-ycmd--params arg)))
    (when (and company-ycmd-insert-arguments params)
      (unless (company-ycmd-ext-templatify params)
        (insert params)
        (company-template-c-like-templatify
         (concat arg params))))))

(advice-add 'company-ycmd--post-completion :override
            #'ptrv/company-ycmd--post-completion)

(provide 'company-ycmd-ext)
;;; company-ycmd-ext ends here
