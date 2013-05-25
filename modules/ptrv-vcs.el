;;; ptrv-vcs.el --- ptrv vsc

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
(defvar pcache-directory
  (let ((dir (file-name-as-directory (concat ptrv-tmp-dir "pcache"))))
    (make-directory dir t)
    dir))

(setq gist-view-gist t)
(eval-after-load 'gist
  '(progn
    (add-to-list 'gist-supported-modes-alist '(processing-mode . "pde"))
    (add-to-list 'gist-supported-modes-alist '(conf-mode . "desktop"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter
(setq git-gutter:window-width 2)

;;(global-git-gutter-mode t)

(setq git-gutter:lighter " G-+")

(setq git-gutter:modified-sign "~ ")
(setq git-gutter:added-sign "+ ")
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:unchanged-sign "  ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;; newline after 72 chars in magit-log-edit-mode
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))

;; http://whattheemacsd.com/setup-magit.el-01.html
;; full screen magit-status
(eval-after-load 'magit
  '(progn
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))
     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen))
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

     (defun magit-toggle-whitespace ()
       (interactive)
       (if (member "-w" magit-diff-options)
           (magit-dont-ignore-whitespace)
         (magit-ignore-whitespace)))

     (defun magit-ignore-whitespace ()
       (interactive)
       (add-to-list 'magit-diff-options "-w")
       (magit-refresh))

     (defun magit-dont-ignore-whitespace ()
       (interactive)
       (setq magit-diff-options (remove "-w" magit-diff-options))
       (magit-refresh))

     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mercurial
(require 'ahg nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-vcs)
;;; ptrv-vcs.el ends here
