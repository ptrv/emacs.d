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
(autoload 'processing-snippets-initialize "processing-mode" nil nil nil)
(eval-after-load 'yasnippet '(processing-snippets-initialize))
(autoload 'processing-find-sketch "processing-mode" nil t)

(cond
 ((eq system-type 'darwin)
  (setq processing-location "processing-java"))
 ((eq system-type 'gnu/linux)
  (setq processing-location "~/applications/processing-2.0/processing-java")
  (setq processing-application-dir "~/applications/processing-2.0")
  (setq processing-sketch-dir "~/processing_sketches_v2")
  ))

(defun processing-mode-init ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary
                     ac-source-yasnippet
                     ac-source-words-in-buffer))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary processing-functions)
  (setq ac-user-dictionary (append ac-user-dictionary processing-builtins))
  (setq ac-user-dictionary (append ac-user-dictionary processing-constants))
  )

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If htmlize is installed, provide this function to copy buffer or
;; region to clipboard
(eval-after-load "processing-mode"
  '(when (and (fboundp 'htmlize-buffer)
              (fboundp 'htmlize-region))
     (defun processing-copy-as-html (&optional arg)
       ""
       (interactive "P")
       (if (eq (buffer-local-value 'major-mode (get-buffer (current-buffer)))
               'processing-mode)
           (save-excursion
             (let ((htmlbuf (if (region-active-p)
                                (htmlize-region (region-beginning) (region-end))
                              (htmlize-buffer))))
               (if arg
                   (switch-to-buffer htmlbuf)
                 (progn
                   (with-current-buffer htmlbuf
                     (clipboard-kill-ring-save (point-min) (point-max)))
                   (kill-buffer htmlbuf)
                   (message "Copied as HTML to clipboard")))))
         (message (concat "Copy as HTML failed, because current "
                          "buffer is not a Processing buffer."))))
     (define-key processing-mode-map (kbd "C-c C-p H") 'processing-copy-as-html)
     (easy-menu-add-item processing-mode-menu nil (list "---"))
     (easy-menu-add-item processing-mode-menu nil
                         ["Copy as HTML" processing-copy-as-html
                          :help "Copy buffer or region as HTML to clipboard"])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-processing)
;;; ptrv-processing.el ends here
