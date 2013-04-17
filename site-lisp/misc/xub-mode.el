;;; xub-mode.el --- Major mode for browsing unicode characters. -*- coding: utf-8 -*-

;; Copyright © 2010 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2010-06-20
;; Keywords: unicode, character map

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either GPL version 2 or 3.

;;; DESCRIPTION

;; A major mode for browsing unicode characters

;;; INSTALL

;; Open the file, then type Alt+x eval-buffer.  You are done.  When
;; you need to browse a file full of unicode characters, just type M-x
;; xub-mode.

;; To install, place this file in the directory 〔~/.emacs.d/〕. Create that dir if it doesn't exist.

;; Then, place the following code in your emacs init file, 〔~/.emacs〕.

;; ;; set auto loading of xub-mode when xub-mode is called.
;; (autoload 'xub-mode "xub-mode" "Load xub-mode for browsing Unicode." t)

;; ;;;; Optional
;; ;; define alias so the command name to invoke mode is more intuitive and easier to remember
;; ; (defalias 'unicode-browser 'xub-mode)

;; Then, restart emacs.

;;; DOCUMENTATION

;; When this mode is on, pressing the arrow keys will automatically
;; display character info of the char under cursor.
;; Mouse left click on a char also works.

;; Get unicode files to browse at: http://ergoemacs.org/emacs/unicode-browser.html

;; To see the inline documentation in emacs, type “C-h m”
;; (describe-mode). (if you have not load the mode, first type
;; Alt+x xub-mode)

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; HISTORY

;; version 1.1.5, 2012-06-30 • minor implementation improvement on {“xub-show-down”, “xub-show-up”}. It no longer generates compilation warning about calling “next-line”, “previous-line”.
;; version 1.1.4, 2010-12-20 • The keys 【Alt+12】 for zoom-in and 【Alt+11】 for zoom-out are removed. New keys are: 【Ctrl++】 (zoom-in) and 【Ctrl+-】 zoom-out. Holding down Ctrl and scroll mouse wheel also works. These are more compatible with web browsers.
;; version 1.1.3, 2010-11-18 • Fixed a bug where describe-major-mode generates a error.
;; version 1.1.2, 2010-08-13 • renamed file frome “xub-unicode-browser-mode.el” to “xub-mode.el”. The feature name is also renamed from “xub-unicode-browser-mode” to “xub-mode”.
;; version 1.1.1, 2010-06-21 • Added xub-forward-to-space and xub-backward-to-space.
;; version 1.1, 2010-06-20 • Fixed first-mouse-click problem. (changed keybinding from "<down-mouse-1>" to "<mouse-1>"). • Added a menu. • Added commands xub-zoom-in xub-zoom-out.
;; version 1.0, 2010-06-20 First version.

;;; Code:

(defvar xub-version)
(setq xub-version "1.1.4")

(defvar xub-map nil "Keymap for xub")

(setq xub-map (make-sparse-keymap))
(define-key xub-map (kbd "<left>") 'xub-show-left)
(define-key xub-map (kbd "<right>") 'xub-show-right)
(define-key xub-map (kbd "<up>") 'xub-show-up)
(define-key xub-map (kbd "<down>") 'xub-show-down)
(define-key xub-map (kbd "<mouse-1>") 'xub-left-click)
(define-key xub-map [remap forward-word] 'xub-forward-to-space)
(define-key xub-map [remap backward-word] 'xub-backward-to-space)
(define-key xub-map (kbd "C-+") 'xub-zoom-in)
(define-key xub-map (kbd "C--") 'xub-zoom-out)
(define-key xub-map (kbd "<C-wheel-up>") 'xub-zoom-in)
(define-key xub-map (kbd "<C-wheel-down>") 'xub-zoom-out)

(define-key xub-map [menu-bar] (make-sparse-keymap))
(let ((menuMap (make-sparse-keymap "XUB")))
    (define-key xub-map [menu-bar xub] (cons "XUB" menuMap))

    (define-key menuMap [about] '("About xub-mode" . xub-about))
    (define-key menuMap [zoom-out] '("Zoom out" . xub-zoom-out))
    (define-key menuMap [zoom-in] '("Zoom in" . xub-zoom-in))
)

(defun xub-forward-to-space ()
  "Move cursor forward to next end of space or newline."
  (interactive)
  (search-forward-regexp " +\\|\n+" nil t)
  )

(defun xub-backward-to-space ()
  "Move cursor backward to previous beginning of space or newline."
  (interactive)
  (search-backward-regexp " +\\|\n+" nil t)
  (skip-chars-backward " ")
  )

(defun xub-about ()
  "Show the author, version number, and description about this package."
  (interactive)
  (with-output-to-temp-buffer "*About xub-mode*"
    (princ
     (concat "Mode name: xub-mode.\n\n"
             "Author: Xah Lee\n\n"
             "Version: " xub-version "\n\n"
             "To see inline documentation, type “Alt+x describe-mode” while you are in xub-mode.\n\n"
             "Home page: URL `http://ergoemacs.org/emacs/unicode-browser.html' \n\n")
     )
    )
  )

(defun xub-zoom-in ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 1)
  )

(defun xub-zoom-out ()
  "Make font size smaller."
  (interactive)
  (text-scale-decrease 1)
  )

(defun xub-left-click ()
  "Show info about the character under cursor."
  (interactive)
  (xub-display-info)
  )

(defun xub-show-right ()
  "Move cursor forward then show info about the character under cursor."
  (interactive)
 (forward-char)
 (xub-display-info)
  )

(defun xub-display-info ()
  "Display char info."
(describe-char (point))
 ;; (if (looking-at " \\|\n")
 ;;     (message "space or newline")
 ;;   (describe-char (point)) )
  )

(defun xub-show-left ()
  "Move cursor backward then show info about the character under cursor."
  (interactive)
 (backward-char)
 (xub-display-info)
  )

(defun xub-show-up ()
  "Move cursor up then show info about the character under cursor."
  (interactive)
  (call-interactively 'previous-line)
  (xub-display-info)
  )

(defun xub-show-down ()
  "Move cursor down then show info about the character under cursor."
  (interactive)
  (call-interactively 'next-line)
  (xub-display-info)
  )

(defun xub-mode ()
  "Major mode for browsing unicode characters.

When this mode is on, pressing the arrow keys will move
cursor and display info about the character under cursor.
Pressing mouse left button on a character also works.

The info will contain the character's unicode code point in
decimal, octal, hexadecimal, and its unicode name, unicode
category, font used, case class (lower/upper), etc.

You can get files unicode character files at:
  URL `http://ergoemacs.org/emacs/unicode-browser.html'

Tips:

In emacs 23.x, to insert a unicode by name or by hex code, call
`ucs-insert'. You can use the tab and * wildcard for name completion.

You need emacs 23.x to enjoy this mode. Because emacs 22's
`describe-char' does not provide full unicode info."
  (interactive)
;  (kill-all-local-variables)
  
  (setq major-mode 'xub-mode)
  (setq mode-name "XUB Unicode Browser")
  (use-local-map xub-map)
  
  (run-mode-hooks 'xub-hook))

(provide 'xub-mode)
