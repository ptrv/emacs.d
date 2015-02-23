;;; ptrv-osx.el --- ptrv's osx config                -*- lexical-binding: t; -*-

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
        prefix)))

(defun ptrv/copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun ptrv/paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'ptrv-osx)
;;; ptrv-osx.el ends here
