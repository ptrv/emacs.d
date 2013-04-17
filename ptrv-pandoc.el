;;; ptrv-pandoc.el --- pandoc-conf

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
;; pandoc-conf.el

(autoload 'turn-on-pandoc "pandoc-mode" nil t)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(custom-set-variables
 '(pandoc-binary "/usr/local/bin/pandoc"))

(add-to-list 'auto-mode-alist '("\\.text$" .  markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-pandoc)
;;; ptrv-pandoc.el ends here
