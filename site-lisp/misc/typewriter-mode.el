;;; typewriter-mode.el --- play typewriter sounds when typing

;; Copyright (C) 2013  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
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

;; Minor mode for playing typewriter sounds when typing.

;;; Code:

(defcustom typewriter-sound-default ""
  "Typing sound file path")
(defcustom typewriter-sound-end ""
  "End of line sound file path")
(defcustom typewriter-sound-return ""
  "Carriage return sound file path")
(defcustom typewriter-end-column 80
  "End of line column number")

(defcustom typewriter-play-command "aplay"
  "Sound player command")

;; Play typing sound
(defun play-typewriter-sound ()
  (if (equal (this-command-keys) (kbd "RET"))
      nil
    (start-process-shell-command "typewriter" nil
                                 (concat
                                  typewriter-play-command
                                  " "
                                  typewriter-sound-default
                                  ))))

;; Play bell when cursor is at column 80
(defun play-typewriter-end ()
  (current-column) typewriter-end-column
  (start-process-shell-command "typewriter" nil
                               (concat
                                typewriter-play-command
                                " "
                                typewriter-sound-end
                                )))

;; Return sound
(defun play-typewriter-return ()
  (if (equal (this-command-keys) (kbd "RET"))
      (start-process-shell-command "typewriter" nil
                                   (concat
                                    typewriter-play-command
                                    " "
                                    typewriter-sound-return
                                    ))))

(defun typewriter-mode-turn-on ()
  (add-hook 'post-self-insert-hook 'play-typewriter-sound)
  (add-hook 'post-command-hook 'play-typewriter-end)
  (add-hook 'post-command-hook 'play-typewriter-return)
  )

(defun typewriter-mode-turn-off ()
  (remove-hook 'post-self-insert-hook 'play-typewriter-sound)
  (remove-hook 'post-command-hook 'play-typewriter-end)
  (remove-hook 'post-command-hook 'play-typewriter-return)
  )

(define-minor-mode typewriter-mode
  "Toggle typewriter-mode"
  nil
  " tt"
  ;; keymap
  nil
  :global t
  (if typewriter-mode
      (typewriter-mode-turn-on)
    (typewriter-mode-turn-off))
  )

(provide 'typewriter-mode)
;;; typewriter-mode.el ends here
