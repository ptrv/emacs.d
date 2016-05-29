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

(defun helm-smex-init ()
  (unless smex-initialized-p
    (smex-initialize))
  (and smex-auto-update
       (smex-detect-new-commands)
       (smex-update)))

(defun helm-smex-action (command)
  (let ((prefix-arg current-prefix-arg))
    (command-execute command 'record)
    (smex-rank command)))

(defclass helm-smex-source (helm-source-sync)
  ((init :initform 'helm-smex-init)
   (candidates :initform 'smex-ido-cache)
   (match :initform 'helm-fuzzy-match)
   (action :initform 'helm-smex-action)
   (coerce :initform 'intern)))

(defun helm-smex ()
  (interactive)
  (let ((helm--mode-line-display-prefarg t))
    (helm :buffer "*helm-smex*"
          :sources (helm-make-source "Smex" helm-smex-source))))

(provide 'ptrv-helm-smex)
;;; ptrv-helm-smex.el ends here
