;;; ptrv-erc.el --- erc-conf

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

(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode ((,freenode-user . ,freenode-pass)))))

;;IRC
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#clojure" "#overtone" "#go-nuts")))

(cond ((string= system-name "alderaan")
       (setq erc-autojoin-channels-alist
             (list (append (car erc-autojoin-channels-alist)
                           '("#supercollider" "#crunchbang" "#debian")))))
      ((string= system-name "anoth")
       (setq erc-autojoin-channels-alist
             (list (append (car erc-autojoin-channels-alist)
                           '("#supercollider" "#ubuntu")))))
      ;; (t (setq erc-autojoin-channels-alist
      ;;          '(("freenode.net" "#emacs" "#clojure" "overtone"))))
      )


(defun erc-connect ()
  "Start up erc and connect to freedonde"
  (interactive)
  (erc :server "irc.freenode.net"
       :full-name "Peter V."
       :port 6667
       :nick freenode-user
       ))

(setq erc-keywords `(,freenode-user))
(erc-match-mode)

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;;change wrap width when window is resized
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
           '(lambda ()
              (save-excursion
                (walk-windows
                 (lambda (w)
                   (let ((buffer (window-buffer w)))
                     (set-buffer buffer)
                     (when (eq major-mode 'erc-mode)
                       (setq erc-fill-column (- (window-width w) 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-erc)
;;; ptrv-erc.el ends here