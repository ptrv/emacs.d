;;; ptrv-flycheck.el --- flycheck-conf

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

(unless (and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
  (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
  (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
  (add-to-list 'debug-ignored-errors "\\`Configured syntax checker .* cannot be used\\'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'emacs-lisp-mode-hook #'(lambda () (flycheck-mode -1)))
(add-hook 'lisp-interaction-mode #'(lambda () (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after 'flycheck
  (setq flycheck-highlighting-mode 'lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-flycheck)
;;; ptrv-flycheck.el ends here
