;;; ptrv-linux.el --- linux-conf

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

(set-frame-font "Inconsolata-12" nil t)
(autoload 'pcomplete/apt-get "pcmpl-apt" nil nil)

(defun setup-frame-hook (frame)
  ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
  ;;(run-with-idle-timer 0.2 nil 'toggle-fullscreen)
  )
(add-hook 'after-make-frame-functions 'setup-frame-hook)

;; erc notification
(after 'erc
  (defun my-notify-erc (match-type nickuserhost message)
    "Notify when a message is received."
    (unless (posix-string-match "^\\** *Users on #" message)
      (notify (format "%s in %s"
                      ;; Username of sender
                      (car (split-string nickuserhost "!"))
                      ;; Channel
                      (or (erc-default-target) "#unknown"))
              ;; Remove duplicate spaces
              (replace-regexp-in-string " +" " " message)
              ;; :icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
              :timeout -1)))

  (add-hook 'erc-text-matched-hook 'my-notify-erc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typeriter-mode
(autoload 'typewriter-mode "typewriter-mode" nil t)
(setq typewriter-play-command "paplay %s")
(setq typewriter-sound-default (concat
                                ptrv-etc-dir
                                "sounds/9744__horn__typewriter.wav"))
(setq typewriter-sound-end (concat
                            ptrv-etc-dir
                            "sounds/eol-bell.wav"))
(setq typewriter-sound-return (concat
                               ptrv-etc-dir
                               "sounds/carriage-return.wav"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-linux)
;;; ptrv-linux.el ends here
