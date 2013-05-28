;;; ptrv-popwin.el --- popwin-conf

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
;; popwin-conf.el

(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd " C-z") popwin:keymap)

;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 30 :stick t)
        ("*Completions*" :noselect t)
        ("*compilation*" :noselect t)
        ("*Messages*")
        ("*Occur*" :noselect t)
        ("\\*Slime Description.*" :noselect t :regexp t :height 30)
        ("*magit-commit*" :noselect t :height 30 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 30 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("*magit-process*" :noselect t :height 15 :width 80)
        ("\\*Slime Inspector.*" :regexp t :height 30)
        ("*Ido Completions*" :noselect t :height 30)
        ;;("*eshell*" :height 20)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*Gofmt Errors*" :noselect t)
        ("\\*godoc" :regexp t :height 30)
        ("*Shell Command Output*" :noselect t)
        ("*nrepl-error*" :height 20 :stick t)
        ("*nrepl-doc*" :height 20 :stick t)
        ("*nrepl-src*" :height 20 :stick t)
        ("*Kill Ring*" :height 30)
        ("*project-status*" :noselect t)
        ("*Compile-Log" :height 20 :stick t)
        ("*pytest*" :noselect t)
        ("*Python*" :stick t)
        ("*Python Doc*" :noselect t)
        ("*jedi:doc*" :noselect t)
        ("*Registers*" :noselect t)
        ))


(provide 'ptrv-popwin)
;;; ptrv-popwin.el ends here
