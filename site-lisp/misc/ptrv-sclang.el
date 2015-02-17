;;; ptrv-sclang.el --- ptrv's sclang extensions      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Keywords: extensions

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

(defun ptrv/sclang-mode-loader ()
  "Load sclang-mode."
  (unless (featurep 'sclang)
    (if (require 'sclang nil t)
        (sclang-mode)
      (message "SCLang is not installed!"))
    (ptrv/sclang-mode-loader--remove)))

(defun ptrv/sclang-mode-loader--remove ()
  "Remove `ptrv/sclang-mode-loader' from `auto-mode-alist'."
  (delete (rassq 'ptrv/sclang-mode-loader auto-mode-alist)
          auto-mode-alist))

(defun ptrv/sclang-start ()
  "Start sclang-mode."
  (interactive)
  (if (require 'sclang nil t)
      (progn
        (sclang-start)
        (ptrv/sclang-mode-loader--remove))
    (message "SCLang is not installed!")))

(with-eval-after-load 'sclang
  (defun ptrv/sclang-all-windows-to-front ()
    "Raise all supercollider windows."
    (interactive)
    (sclang-eval-string "Window.allWindows.do(_.front);"))

  (defun ptrv/sclang-ido-switch-to-buffer ()
    (interactive)
    (let* ((blist (buffer-list))
           (predicate (lambda (b)
                        (with-current-buffer b
                          (derived-mode-p 'sclang-mode))))
           (sc-buffers (delq nil (mapcar
                                  (lambda (b)
                                    (if (funcall predicate b) b nil))
                                  blist))))
      (pop-to-buffer-same-window
       (ido-completing-read "SCLang buffers: "
                            (mapcar 'list (mapcar 'buffer-name sc-buffers))))))

  (defun ptrv/sclang--show-window (win-cmd-str transparent? &optional alpha)
    ""
    (let* ((alpha-val (or alpha 0.6))
           (transparency-code (if transparent?
                                  (concat
                                   ".window.alpha = "
                                   (number-to-string alpha-val)))))
      (sclang-eval-string (concat win-cmd-str transparency-code ";"))))

  (defun ptrv/sclang-show-meter (arg)
    "Show level meter."
    (interactive "P")
    (ptrv/sclang--show-window "Server.default.meter" arg))
  (defun ptrv/sclang-show-scope (arg)
    "Show scope."
    (interactive "P")
    (ptrv/sclang--show-window "Server.default.scope(numChannels: 2)" arg))
  (defun ptrv/sclang-show-helper-window (arg)
    "Show helper window."
    (interactive "P")
    (ptrv/sclang--show-window "HelperWindow.new" arg))
  (defun ptrv/sclang-show-node-tree ()
    "Show tree window."
    (interactive)
    (sclang-eval-string "Server.default.plotTree;"))

  (defvar sclang-mode-map)
  (let ((map sclang-mode-map))
    (define-key map (kbd "C-c F") 'ptrv/sclang-all-windows-to-front)
    (define-key map (kbd "C-c C-b") 'ptrv/sclang-ido-switch-to-buffer))

  (defvar sclang-server-key-map)
  (let ((map sclang-server-key-map))
    (define-key map [?l] 'ptrv/sclang-show-meter)
    (define-key map [?s] 'ptrv/sclang-show-scope)
    (define-key map [?h] 'ptrv/sclang-show-helper-window)
    (define-key map [?t] 'ptrv/sclang-show-node-tree)))

(provide 'ptrv-sclang)
;;; ptrv-sclang.el ends here
