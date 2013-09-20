;;; sql-spatialite-ext.el --- Spatialite extensions for sql.el -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords: sql, spatialite

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

;; Spatialite extesnsions for sql.el.

;; Add this file to your load path and put the following in your .emacs:
;; (autoload 'sql-spatialite "sql-spatialite-ext"
;;  "Run spatialite as an inferior process." t)

;;; Code:

(require 'sql)

(defcustom sql-spatialite-program (or (executable-find "spatialite")
                                      "spatialite")
  "Command to start Spatialite.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-spatialite-options nil
  "List of additional options for `sql-spatialite-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-spatialite-login-params
  '((database :file ".*\\.\\(db\\|sqlite[23]?\\)"))
  "List of login parameters needed to connect to Spatialite."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

(add-to-list 'sql-product-alist
             '(spatialite
               :name "Spatialite"
               :free-software t
               :font-lock sql-mode-sqlite-font-lock-keywords
               :sqli-program sql-spatialite-program
               :sqli-options sql-spatialite-options
               :sqli-login sql-spatialite-login-params
               :sqli-comint-func sql-comint-sqlite
               :list-all ".tables"
               :list-table ".schema %s"
               :completion-object sql-sqlite-completion-object
               :prompt-regexp "^spatialite> "
               :prompt-length 12
               :prompt-cont-regexp "^   \.\.\.> "
               :terminator ";"))

;;;###autoload
(defun sql-spatialite (&optional buffer)
  "Run spatialite as an inferior process.

Spatialite is free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-spatialite-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-spatialite-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-spatialite].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-spatialite].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'spatialite buffer))

(provide 'sql-spatialite-ext)

;;; sql-spatialite-ext.el ends here
