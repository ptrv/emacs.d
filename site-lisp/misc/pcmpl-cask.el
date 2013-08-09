;;; pcmpl-cask.el --- Pcomplete plugin for Cask.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Peter Vasil

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

(defconst pcmpl-cask-commands
  '("package-directory" "path" "load-path" "help" "info" "list"
    "version" "init" "exec" "update" "install" "package")
  "List of `cask' commands")

(defun pcomplete/cask ()
  "Completion for `cask'"
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-cask-commands)

  ;; complete files/dirs forever if the command is `add' or `rm'.
  (cond
   ((pcomplete-match (regexp-opt '("exec")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ((pcomplete-match (regexp-opt '("init")))
    (pcomplete-here (list "--dev")))))

(provide 'pcmpl-cask)
;;; pcmpl-cask.el ends here
