;;; ptrv-backup.el --- ptrv backup conf

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
(require 'backup-dir)
(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      `((t ,ptrv-backups-dir ok-create full-path prepend-name)))

(setq auto-save-file-name-transforms `((".*" ,(concat ptrv-autosaves-dir "\\1") t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,ptrv-backups-dir)))
(setq auto-save-list-file-name (concat ptrv-autosaves-dir "autosave-list"))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(provide 'ptrv-backup)
;;; ptrv-backup.el ends here
