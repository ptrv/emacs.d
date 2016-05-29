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
(require 'helm)
(require 'helm-source)

;; (defvar helm-smex-source--cache (make-hash-table :test #'eq))

;; (defun helm-smex-score-no-cache (command)
;;   (or (cdr (assoc command smex-cache)) 0))

;; (defun helm-smex-score (command)
;;   (or (gethash command helm-smex-source--cache)
;;       (puthash command (helm-smex-score-no-cache command)
;;                helm-smex-source--cache)))

;; (defun helm-smex-compare-candidates (command-name1 command-name2)
;;   (> (helm-smex-score (intern-soft command-name1))
;;      (helm-smex-score (intern-soft command-name2))))

;; (defun helm-smex-sort (candidates source)
;;   (message "bla")
;;   (sort candidates #'helm-smex-compare-candidates))

(defun helm-smex-init ()
  (unless smex-initialized-p
    (smex-initialize))
  (and smex-auto-update
       (smex-detect-new-commands)
       (smex-update))
  (clrhash helm-smex-source--cache))

(defun helm-smex-action (command-name)
  (unwind-protect
      (execute-extended-command current-prefix-arg
                                command-name)
    (smex-rank (intern command-name))))

(defclass helm-smex-source (helm-source-sync)
  ((init :initform 'helm-smex-init)
   (candidates :initform 'smex-ido-cache)
   (match :initform 'helm-fuzzy-match)
   ;; (filtered-candidate-transformer
   ;;  :initform 'helm-smex-sort)
   (action :initform 'helm-smex-action)))

(defun helm-smex ()
  (interactive)
  (let ((helm--mode-line-display-prefarg t))
    (helm :buffer "*helm-smex*"
          :sources (helm-make-source "Smex" helm-smex-source))))

(provide 'ptrv-helm-smex)
;;; ptrv-helm-smex.el ends here
