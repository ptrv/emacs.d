;;; ptrv-processing.el --- processing-conf

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

(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(cond
 ((eq system-type 'darwin)
  (setq processing-location "processing-java"))
 ((eq system-type 'gnu/linux)
  (setq processing-location "~/applications/processing-2.0/processing-java"))
 )


;;(yas-load-directory (concat (live-pack-lib-dir) "processing2-emacs/snippets"))

(add-hook 'processing-mode-hook
          (lambda ()
            (setq ac-sources
                  '(ac-source-yasnippet
                    ;;ac-source-dictionary
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ;;ac-source-words-in-all-buffer
                    ;;ac-source-semantic
                    ))))

(add-to-list 'ac-modes 'processing-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-processing)
;;; ptrv-processing.el ends here
