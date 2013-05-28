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

;; (if (display-graphic-p)
;;     (progn
;;       (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))))
(set-frame-font "Inconsolata-12" nil t)
(autoload 'pcomplete/apt-get "pcmpl-apt" nil nil)

(defun setup-frame-hook (frame)
  ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
  ;;(run-with-idle-timer 0.2 nil 'toggle-fullscreen)
  )
(add-hook 'after-make-frame-functions 'setup-frame-hook)

;; Define a function for making desktop notifications
(autoload 'dbus-call-method "dbus" nil nil)
(defun dbus-send-desktop-notification (summary body icon timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0 icon summary body
    '(:array)
    '(:array (:dict-entry "x-canonical-append" (:variant "allowed")))
    ':int32 timeout))

;; ERC stuff

;; (require 'notifications)
;; (defun erc-global-notify (match-type nick message)
;;   "Notify when a message is recieved."
;;   (unless (posix-string-match "^\\** *Users on #" message)
;;     (notifications-notify
;;      :title (format "%s in %s"
;;                     ;; Username of sender
;;                     (car (split-string nick "!"))
;;                     ;; Channel
;;                     (or (erc-default-target) "#unknown"))
;;      :body message
;;      )))

;; (add-hook 'erc-text-matched-hook 'erc-global-notify)

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

(add-hook 'erc-text-matched-hook 'my-notify-erc)

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
