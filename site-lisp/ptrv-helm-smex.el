;;; ptrv-helm-smex.el --- ptrv's helm-smex config    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords: convenience

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

(require 'smex)
(require 'helm-source)

(defvar helm-smex-source--cache (make-hash-table :test #'eq))

(defun helm-smex-score-no-cache (command)
  (or (cdr (car (cl-member (symbol-name command) smex-cache
                           :test #'string=)))
      0))

(defun helm-smex-score (command)
  (or (gethash command helm-smex-source--cache)
      (puthash command (helm-smex-score-no-cache command)
               helm-smex-source--cache)))

(defun helm-smex-compare-candidates (command-name1 command-name2)
  (> (helm-smex-score (intern-soft command-name1))
     (helm-smex-score (intern-soft command-name2))))

(defun helm-smex-items ()
  (unless smex-initialized-p
    (smex-initialize))
  (and (smex-detect-new-commands)
       (smex-update))
  smex-ido-cache)

(defclass helm-smex-source (helm-source-sync)
  ((init
    :initform (lambda ()
                (clrhash helm-smex-source--cache)))
   (candidates :initform 'helm-smex-items)
   (match :initform 'helm-fuzzy-match)
   (filtered-candidates-transformer
    :initform (lambda (candidates source)
                (sort candidates #'helm-smex-compare-candidates)))
   (action
    :initform (lambda (command-name)
                (unwind-protect
                    (execute-extended-command current-prefix-arg
                                              command-name)
                  (smex-rank (intern command-name)))))))

(defun helm-smex ()
  (interactive)
  (helm :buffer "*helm-smex*"
        :sources (helm-make-source "Smex" helm-smex-source)))

(provide 'ptrv-helm-smex)
;;; ptrv-helm-smex.el ends here
