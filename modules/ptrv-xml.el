;;; ptrv-xml.el --- xml-conf

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

(add-to-list 'auto-mode-alist '("\\.gpx$" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'nxml-mode
  '(progn
     ;; make nxml outline work with gpx files
     (defun gpx-setup ()
       (when (and (stringp buffer-file-name)
                  (string-match "\\.gpx\\'" buffer-file-name))
         (make-local-variable 'nxml-section-element-name-regexp)
         (setq nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
         (make-local-variable 'nxml-heading-element-name-regexp)
         (setq nxml-heading-element-name-regexp "name\\|time")
         ))
     (add-hook 'nxml-mode-hook 'gpx-setup)

     ;; typing a slash automatically completes the end-tag
     (setq nxml-slash-auto-complete-flag t)

     ;; treat an element as a single expression instead of only tag
     (setq nxml-sexp-element-flag t)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-xml)
;;; ptrv-xml.el ends here
