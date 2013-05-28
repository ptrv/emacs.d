;;; ptrv-bindings.el --- ptrv bindings conf

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

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x f") 'ido-recentf-open)
;; (global-set-key (kbd "C-c p f") 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'idomenu)

(global-set-key [f5] 'refresh-file)

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)
(global-set-key [f9] 'delete-other-windows)

(global-set-key (kbd "C-x g") 'magit-status)

;;mark current function
(global-set-key (kbd "C-x C-p") 'mark-defun)

;;emacs-lisp shortcuts
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "C-c m s") 'eval-and-replace) ;swap
(global-set-key (kbd "C-c m b") 'eval-buffer)
(global-set-key (kbd "C-c m e") 'eval-last-sexp)
(global-set-key (kbd "C-c m i") 'eval-expression)
(global-set-key (kbd "C-c m d") 'eval-defun)
(global-set-key (kbd "C-c m n") 'eval-print-last-sexp)
(global-set-key (kbd "C-c m r") 'eval-region)

;;diff shortcuts
(global-set-key (kbd "C-c d f") 'diff-buffer-with-file)

(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)

(global-set-key (kbd "C-c w .") (lambda () (interactive) (shrink-window-horizontally 4)))
(global-set-key (kbd "C-c w ,") (lambda () (interactive) (enlarge-window-horizontally 4)))
(global-set-key (kbd "C-c w <down>") (lambda () (interactive) (enlarge-window -4)))
(global-set-key (kbd "C-c w <up>") (lambda () (interactive) (enlarge-window 4)))

;; winner undo and redo
(global-set-key (kbd "C-c w b") 'winner-undo)
(global-set-key (kbd "C-c w f") 'winner-redo)

;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-C-%") 'query-replace)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Ace jump mode
(after 'ace-jump-mode-autoloads
  (global-set-key (kbd "C-o") 'ace-jump-mode))

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)
(after 'nrepl
  (define-key nrepl-mode-map (kbd "M-RET") 'nrepl-doc)
  (define-key nrepl-interaction-mode-map (kbd "M-RET") 'nrepl-doc))

;; Make Emacs use "newline-and-indent" when you hit the Enter key so
;; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m" 'newline-and-indent)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))


;; iflipb
(after 'iflipb-autoloads
  (global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
  (global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
  (global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
  (global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer))

(define-key ac-completing-map "\r" 'ac-complete)

(after 'browse-kill-ring-autoloads
  (browse-kill-ring-default-keybindings))

;; kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'comment-dwim-line)

;; multiple-cursors
(after 'multiple-cursors-autoloads
  ;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-ö") 'mc/edit-lines)
  (global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-ö") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-ä") 'mc/mark-all-like-this))

;; expand-region
(after 'expand-region-autoloads
  (global-set-key (kbd "C-ü") 'er/expand-region)
  (global-set-key (kbd "C-Ü") 'er/contract-region))

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "<f12>") 'google)
(global-set-key (kbd "S-<f12>") 'webjump)

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region-below)

(global-set-key (kbd "C-S-M-d") 'duplicate-line-below-comment)

(global-set-key (kbd "C-c q") 'exit-emacs-client)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(global-set-key (kbd "<C-M-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(after 'sql
  (define-key sql-mode-map (kbd "C-c C-p p") 'sql-set-product)
  (define-key sql-mode-map (kbd "C-c C-p i") 'sql-set-sqli-buffer)
  (define-key sql-mode-map (kbd "C-c C-p s") 'sql-switch-spatialite-sqlite))

;; http://irreal.org/blog/?p=1742
(global-set-key (kbd "C-c t")
                #'(lambda ()
                    "Bring up a full-screen eshell or restore previous config."
                    (interactive)
                    (if (string= "eshell-mode" major-mode)
                        (jump-to-register :eshell-fullscreen)
                      (progn
                        (window-configuration-to-register :eshell-fullscreen)
                        (eshell)
                        (delete-other-windows)))))

(global-set-key (kbd "M-j") (lambda ()
                              (interactive)
                              (join-line -1)))

;; (global-set-key (kbd "<f11>") 'toggle-fold)
(global-set-key (kbd "<f11>") 'hs-toggle-hiding)
(global-set-key (kbd "S-<f11>") 'toggle-fold-all)

;; autopair-newline interferes with cua-rotate-rectangle (default binding "\r")
(after 'cua-rect
  (define-key cua--rectangle-keymap (kbd "M-<return>") 'cua-rotate-rectangle))

(after 'flyspell
  (define-key flyspell-mode-map (kbd "C-:") 'flyspell-auto-correct-word)
  (define-key flyspell-mode-map (kbd "C-.") 'ispell-word))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "C-M-z") 'indent-defun)

(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;;http://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
(global-set-key (kbd "C-x O") #'(lambda ()
                                  (interactive)
                                  (other-window -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; registers
(global-set-key (kbd "C-x r T") 'string-insert-rectangle)
(global-set-key (kbd "C-x r v") 'ptrv-list-registers)

(set-register ?e '(file . "~/.emacs.d"))
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?m '(file . "~/.emacs.d/modules"))
(set-register ?o '(file . "~/Dropbox/org/newgtd.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key Info-mode-map "ä" 'Info-forward-node)
(define-key Info-mode-map "ö" 'Info-backward-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-chord
(after 'key-chord-autoloads
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'switch-to-previous-buffer)
  (key-chord-define-global "KK" 'winner-undo)
  (key-chord-define-global "LL" 'winner-redo)
  (key-chord-define-global "BB" 'ido-switch-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'visit-ielm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<C-f9>") #'global-git-gutter-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key (kbd "C-c I") 'find-user-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-bindings)
;;; ptrv-bindings.el ends here
