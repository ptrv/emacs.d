;;; ptrv-x11.el --- X11-conf

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

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;; set fullscreen if we are on a small display
;; (if (and
;;      (<= (x-display-pixel-width) 1280)
;;      (<= (x-display-pixel-height) 800))
;;     (toggle-fullscreen))

(cond ((and
         (<= (x-display-pixel-width) 1280)
         (<= (x-display-pixel-height) 800))
       (toggle-fullscreen))
      (t
       ;; (set-frame-size (selected-frame) 110 60)
       (set-frame-size (selected-frame) 130 60)
       (set-frame-position (selected-frame) 500 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ptrv-x11)
;;; ptrv-x11.el ends here
