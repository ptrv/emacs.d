;;; ptrv-erc.el --- ptrv's ERC config                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Peter Vasil

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

;;

;;; Code:

(require 'erc)
(require 'erc-services)
(require 'erc-track)
(require 'erc-match)


;; general

(add-hook 'erc-mode-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      (lambda ()
                        (setq erc-fill-column (- (window-width) 2)))
                      nil :local)))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(add-to-list 'erc-modules 'spelling)
(erc-update-modules)


;; erc-services

(defun ptrv/get-nickserv-password (network)
  (let ((creds (netrc-credentials network)))
    (cadr creds)))

(defun ptrv/erc-nickserv-identify (orig-func &rest args)
  (let* ((arg (car args))
         (password (if (functionp arg)
                       (funcall arg)
                     arg)))
    (apply orig-func (list password))))

(advice-add 'erc-nickserv-identify :around
            #'ptrv/erc-nickserv-identify)

(erc-services-mode)
(setq erc-prompt-for-nickserv-password nil)

(let ((freenode-credentials (netrc-credentials "freenode"))
      (oftc-credentials (netrc-credentials "oftc")))
  (setq erc-nickserv-passwords
        `((freenode (,(cons (car freenode-credentials)
                            (apply-partially 'ptrv/get-nickserv-password
                                             "freenode"))))
          (oftc (,(cons (car oftc-credentials)
                        (apply-partially 'ptrv/get-nickserv-password
                                         "oftc")))))))


;; erc-track

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))


;; erc-match

(with-eval-after-load 'alert
  (add-hook 'erc-text-matched-hook
            (lambda (match-type nickuserhost message)
              "Notify when a message is received."
              (unless (posix-string-match "^\\** *Users on #" message)
                (alert (replace-regexp-in-string " +" " " message)
                       :title (format "%s in %s"
                                      ;; Username of sender
                                      (car (split-string nickuserhost "!"))
                                      ;; Channel
                                      (or (erc-default-target) "#unknown")))))))


;; other

(defun ptrv/erc-switch-to-buffer (&optional arg)
  "Prompt for a ERC buffer to switch to.
When invoked with prefix argument, use all erc buffers.  Without prefix
ARG, allow only buffers related to same session server.
If `erc-track-mode' is in enabled, put the last element of
`erc-modified-channels-alist' in front of the buffer list."
  (interactive "P")
  (when (bound-and-true-p ido-mode)
    (switch-to-buffer
     (ido-completing-read
      "Switch to ERC buffer: "
      (save-excursion
        (delq
         nil
         (mapcar 'buffer-name
                 (erc-buffer-list
                  nil
                  (when arg erc-server-process)))))
      nil t nil nil
      (when (boundp 'erc-modified-channels-alist)
        (buffer-name (caar (last erc-modified-channels-alist))))))))


(provide 'ptrv-erc)
;;; ptrv-erc.el ends here
