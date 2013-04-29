;;; ptrv-ibuffer.el --- ibuffer-conf

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
;; ibuffer-conf.el

;;organise ibuffer into handy groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("IRC"      (mode . erc-mode))
               ("emacs" (or (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")
                            (name . "^\\*Completions\\*$")
                            (filename . ".emacs.d")
                            (filename . ".live-packs")))
               ("magit" (name . "\\*magit"))
               ("dired" (mode . dired-mode))
               ("sclang" (mode . sclang-mode))
               ("Org" (mode . org-mode))
               ("Help" (or (name . "\\*Help\\*")
                           (name . "\\*Apropos\\*")
                           (name . "\\*info\\*")))
               ("#!-config" (filename . ".cb-config"))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer-git)
(setq ibuffer-formats
      '((mark modified read-only git-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (git-status 8 8 :left :elide)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-ibuffer)
;;; ptrv-ibuffer.el ends here
