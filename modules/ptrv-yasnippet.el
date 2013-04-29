;;; ptrv-yasnippet.el --- yasnippet-conf

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

;; (setq ptrv-yasnippet-dir (concat dotfiles-dir "snippets"))
;; (setq yas-snippet-dirs `(,ptrv-yasnippet-dir))
(yas-global-mode 1)

;; (defun ptrv-reload-snippets ()
;;   (interactive)
;;   (yas-load-directory yas-snippet-dir))


;; add snippets
;; (setq ptrv-yasnippet-dir (concat (live-pack-lib-dir) "snippets"))
;; (yas-load-directory ptrv-yasnippet-dir)



(eval-after-load 'yasnippet
  '(progn
     (setq yas-keymap  (let ((map (make-sparse-keymap)))
                         (define-key map [(control tab)] 'yas-next-field-or-maybe-expand)
                         (define-key map (kbd "C-TAB")   'yas-next-field-or-maybe-expand)
                         (define-key map [(shift tab)]   'yas-prev-field)
                         (define-key map [backtab]       'yas-prev-field)
                         (define-key map (kbd "C-g")     'yas-abort-snippet)
                         (define-key map (kbd "C-d")     'yas-skip-and-clear-or-delete-char)
                         map))
     (require 'dropdown-list)
     (setq yas-prompt-functions '(yas-dropdown-prompt
                                  yas-ido-prompt
                                  yas-x-prompt
                                  yas-completing-prompt))))

(provide 'ptrv-yasnippet)
;;; ptrv-yasnippet.el ends here
