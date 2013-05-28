;;; ptrv-supercollider.el --- sulercollider-conf

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

(after 'w3m
  (define-key w3m-mode-map [left] 'backward-char)
  (define-key w3m-mode-map [right] 'forward-char)
  (define-key w3m-mode-map [up] 'previous-line)
  (define-key w3m-mode-map [down] 'next-line)
  (setq w3m-auto-show 1)
  (setq w3m-key-binding 'info)
  (setq w3m-pop-up-frames t)
  (setq w3m-pop-up-windows nil))

(defun sclang-ptrv ()
  (interactive)
  (if (require 'sclang nil t)
      (sclang-start)
    (message "SCLang is not installed!")))

(after 'sclang
  (message "sclang config has been loaded !!!")
  (setq sclang-auto-scroll-post-buffer nil
        sclang-eval-line-forward nil
        ;;sclang-help-path '("~/.local/share/SuperCollider/Help")
        sclang-library-configuration-file "~/.sclang.cfg"
        sclang-runtime-directory "~/scwork/"
        sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")

  ;; ##### extension for block error messages ####
  ;;(load-file (concat (live-pack-lib-dir) "ext-scel.el"))

  ;; (add-hook 'sclang-mode-hook
  ;;           (lambda ()
  ;;             (setq ac-sources
  ;;                   '(ac-source-dictionary
  ;;                     ac-source-words-in-buffer
  ;;                     ac-source-words-in-same-mode-buffers
  ;;                     ac-source-words-in-all-buffer
  ;;                     ;;ac-source-yasnippet
  ;;                     ac-source-semantic))))


  (defun sclang-mode-untabify ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

  ;; (require 'ext-scel)
  ;; (setq sclang-minibuf-results nil)
  ;; (setq sclang-collapse t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; hooks

  (defun supercollider-init ()
    (add-to-list 'ac-modes 'sclang-mode)
    (make-local-variable 'ac-user-dictionary-files)
    (add-to-list 'ac-user-dictionary-files "~/.sc_completion")

    ;; (add-to-list 'ac-user-dictionary-files
    ;;              "~/.local/share/SuperCollider/sclang_completion_dict")
    (yas-minor-mode 1)
    (make-local-variable 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'sclang-mode-untabify)

    ;; set buffer local keymap to set TAB for jumping to next button in
    ;; post window when using ext-scel's collapsible post window text.
    (when (string= (buffer-name) sclang-post-buffer)
      (use-local-map (copy-keymap sclang-mode-map))
      (local-set-key [?\t] 'forward-button)
      (local-set-key [backtab] 'backward-button))

    )
  (add-hook 'sclang-mode-hook 'supercollider-init)

  (eval-after-load "sclang"
    '(progn
       (define-key sclang-mode-map (kbd "C-c ö") 'sclang-dump-interface)
       (define-key sclang-mode-map (kbd "C-c ü") 'sclang-dump-full-interface)
       (define-key sclang-mode-map (kbd "C-c ä") 'sclang-pop-definition-mark)
       ;; Raise all supercollider windows.
       (define-key sclang-mode-map (kbd "C-c f")
         (lambda ()
           (interactive)
           (sclang-eval-string "Window.allWindows.do(_.front);")))
       (define-key sclang-server-key-map [?l]
         (lambda ()
           (interactive)
           (sclang-eval-string "Server.default.meter;")))
       (define-key sclang-server-key-map [?s]
         (lambda ()
           (interactive)
           (sclang-eval-string "Server.default.scope(numChannels: 2);")))
       (define-key sclang-server-key-map [?h]
         (lambda ()
           (interactive)
           (sclang-eval-string "HelperWindow.new;")))
       ))

  (if (fboundp 'completing-read-ido)
      (progn
        (add-to-list 'ido-ubiquitous-command-exceptions 'sclang-dump-interface)
        (add-to-list 'ido-ubiquitous-command-exceptions 'sclang-dump-full-interface))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-supercollider)
;;; ptrv-supercollider.el ends here
