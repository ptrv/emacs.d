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

(defgroup typewriter nil
  "Typewriter-mode."
  :group 'multimedia)

(defcustom typewriter-sound-default ""
  "Typing sound file path."
  :group 'typewriter)

(defcustom typewriter-sound-end ""
  "End of line sound file path."
  :group 'typewriter)

(defcustom typewriter-sound-return ""
  "Carriage return sound file path."
  :group 'typewriter)

(defcustom typewriter-end-column 80
  "End of line column number."
  :group 'typewriter)

(defcustom typewriter-play-command nil
  "Sound player command."
  :group 'typewriter)

(defun typewriter-play-sound (sound-file)
  "Play SOUND-FILE."
  (if typewriter-play-command
	  (start-process-shell-command "typewriter-sound" nil
                                   (format typewriter-play-command
                                           sound-file))
	(play-sound-file sound-file)))

;; Play typing sound
(defun play-typewriter-sound ()
  (unless (equal (this-command-keys) (kbd "RET"))
    (typewriter-play-sound typewriter-sound-default)))

;; Play bell when cursor is at column 80
(defun play-typewriter-end ()
  (if (eq (current-column) typewriter-end-column)
      (typewriter-play-sound typewriter-sound-end)))

;; Return sound
(defun play-typewriter-return ()
  (if (equal (this-command-keys) (kbd "RET"))
      (typewriter-play-sound typewriter-sound-return)))

(defun typewriter-mode-turn-on ()
  (add-hook 'post-self-insert-hook 'play-typewriter-sound)
  (add-hook 'post-command-hook 'play-typewriter-end)
  (add-hook 'post-command-hook 'play-typewriter-return))

(defun typewriter-mode-turn-off ()
  (remove-hook 'post-self-insert-hook 'play-typewriter-sound)
  (remove-hook 'post-command-hook 'play-typewriter-end)
  (remove-hook 'post-command-hook 'play-typewriter-return))

(define-minor-mode typewriter-mode
  "Toggle typewriter-mode"
  nil
  " tt"
  ;; keymap
  nil
  :global t
  (if typewriter-mode
      (typewriter-mode-turn-on)
    (typewriter-mode-turn-off)))

(provide 'typewriter-mode)
;;; typewriter-mode.el ends here
