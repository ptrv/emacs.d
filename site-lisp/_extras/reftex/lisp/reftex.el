;;; reftex.el --- Version and autoloads of RefTeX

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains version information for RefTeX and autoloads for
;; functions which should be callable by the user, other packages, or
;; other RefTeX files without having to load the whole file where they
;; are defined unless they are actually needed.

;;; Code:

(defconst reftex-version "2012-10-04"
  "Version string for RefTeX.")

;;; Autoloads


;;;### (autoloads (reftex-add-label-environments) "reftex-auc" "reftex-auc.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-auc.el

(autoload 'reftex-add-label-environments "reftex-auc" "\
Add label environment descriptions to `reftex-label-alist-style'.
The format of ENTRY-LIST is exactly like `reftex-label-alist'.  See there
for details.
This function makes it possible to support RefTeX from AUCTeX style files.
The entries in ENTRY-LIST will be processed after the user settings in
`reftex-label-alist', and before the defaults (specified in
`reftex-default-label-alist-entries').  Any changes made to
`reftex-label-alist-style' will raise a flag to the effect that
the label information is recompiled on next use.

\(fn ENTRY-LIST)" nil nil)

(defalias 'reftex-add-to-label-alist 'reftex-add-label-environments)

;;;***

;;;### (autoloads (reftex-reset-scanning-information reftex-set-cite-format
;;;;;;  reftex-mode turn-on-reftex) "reftex-base" "reftex-base.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-base.el

(autoload 'turn-on-reftex "reftex-base" "\
Turn on RefTeX mode.

\(fn)" nil nil)

(autoload 'reftex-mode "reftex-base" "\
Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX.

\\<reftex-mode-map>A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `\\[reftex-toc]'.

Labels can be created with `\\[reftex-label]' and referenced with `\\[reftex-reference]'.
When referencing, you get a menu with all labels of a given type and
context of the label definition.  The selected label is inserted as a
\\ref macro.

Citations can be made with `\\[reftex-citation]' which will use a regular expression
to pull out a *formatted* list of articles from your BibTeX
database.  The selected citation is inserted as a \\cite macro.

Index entries can be made with `\\[reftex-index-selection-or-word]' which indexes the word at point
or the current selection.  More general index entries are created with
`\\[reftex-index]'.  `\\[reftex-display-index]' displays the compiled index.

Most command have help available on the fly.  This help is accessed by
pressing `?' to any prompt mentioning this feature.

Extensive documentation about RefTeX is available in Info format.
You can view this information with `\\[reftex-info]'.

\\{reftex-mode-map}
Under X, these and other functions will also be available as `Ref' menu
on the menu bar.

------------------------------------------------------------------------------

\(fn &optional ARG)" t nil)

(autoload 'reftex-set-cite-format "reftex-base" "\
Set the document-local value of `reftex-cite-format'.
When such a value exists, it overwrites the setting given with
`reftex-cite-format'.  See the documentation of `reftex-cite-format'
for possible values.  This function should be used from AUCTeX style files.

\(fn VALUE)" nil nil)

(autoload 'reftex-reset-scanning-information "reftex-base" "\
Reset the symbols containing information from buffer scanning.
This enforces rescanning the buffer on next use.

\(fn)" nil nil)

;;;***

;;;### (autoloads (reftex-citation) "reftex-cite" "reftex-cite.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-cite.el

(autoload 'reftex-citation "reftex-cite" "\
Make a citation using BibTeX database files.
After prompting for a regular expression, scans the buffers with
bibtex entries (taken from the \\bibliography command) and offers the
matching entries for selection.  The selected entry is formatted according
to `reftex-cite-format' and inserted into the buffer.

If NO-INSERT is non-nil, nothing is inserted, only the selected key returned.

FORMAT-KEY can be used to pre-select a citation format.

When called with a `C-u' prefix, prompt for optional arguments in
cite macros.  When called with a numeric prefix, make that many
citations.  When called with point inside the braces of a `\\cite'
command, it will add another key, ignoring the value of
`reftex-cite-format'.

The regular expression uses an expanded syntax: && is interpreted as `and'.
Thus, `aaaa&&bbb' matches entries which contain both `aaaa' and `bbb'.
While entering the regexp, completion on knows citation keys is possible.
`=' is a good regular expression to match all entries in all files.

\(fn &optional NO-INSERT FORMAT-KEY)" t nil)

;;;***

;;;### (autoloads (reftex-isearch-minor-mode) "reftex-global" "reftex-global.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-global.el

(autoload 'reftex-isearch-minor-mode "reftex-global" "\
When on, isearch searches the whole document, not only the current file.
This minor mode allows isearch to search through all the files of
the current TeX document.

With no argument, this command toggles
`reftex-isearch-minor-mode'.  With a prefix argument ARG, turn
`reftex-isearch-minor-mode' on if ARG is positive, otherwise turn it off.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (reftex-index-phrases-mode) "reftex-index" "reftex-index.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-index.el

(autoload 'reftex-index-phrases-mode "reftex-index" "\
Major mode for managing the Index phrases of a LaTeX document.
This buffer was created with RefTeX.

To insert new phrases, use
 - `C-c \\' in the LaTeX document to copy selection or word
 - `\\[reftex-index-new-phrase]' in the phrases buffer.

To index phrases use one of:

\\[reftex-index-this-phrase]     index current phrase
\\[reftex-index-next-phrase]     index next phrase (or N with prefix arg)
\\[reftex-index-all-phrases]     index all phrases
\\[reftex-index-remaining-phrases]     index current and following phrases
\\[reftex-index-region-phrases]     index the phrases in the region

You can sort the phrases in this buffer with \\[reftex-index-sort-phrases].
To display information about the phrase at point, use \\[reftex-index-phrases-info].

For more information see the RefTeX User Manual.

Here are all local bindings.

\\{reftex-index-phrases-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (reftex-all-document-files) "reftex-parse" "reftex-parse.el"
;;;;;;  (20589 60300))
;;; Generated autoloads from reftex-parse.el

(autoload 'reftex-all-document-files "reftex-parse" "\
Return a list of all files belonging to the current document.
When RELATIVE is non-nil, give file names relative to directory
of master file.

\(fn &optional RELATIVE)" nil nil)

;;;***

;;;### (autoloads nil "reftex-vars" "reftex-vars.el" (20589 60301))
;;; Generated autoloads from reftex-vars.el
(put 'reftex-vref-is-default 'safe-local-variable (lambda (x) (or (stringp x) (symbolp x))))
(put 'reftex-fref-is-default 'safe-local-variable (lambda (x) (or (stringp x) (symbolp x))))
(put 'reftex-level-indent 'safe-local-variable 'integerp)
(put 'reftex-guess-label-type 'safe-local-variable (lambda (x) (memq x '(nil t))))

;;;***


(provide 'reftex)

;;; reftex.el ends here
