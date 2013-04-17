;;; xmsi-math-symbols-input.el --- a mode to input math chars. -*- coding: utf-8 -*-

;; Copyright © 2010, 2011, 2012, 2013 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2010-12-08
;; Keywords: math symbols, unicode, input

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2.

;;; DESCRIPTION

;; A minor mode for inputing math symbols and Unicode symbols.
;; For download location and documentation, see:
;; http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;;; INSTALL

;; Open the file, then type 【Alt+x eval-buffer】. That's it.

;; To have emacs automatically load the file when it restarts, follow these steps:

;; ① Rename the file to 〔xmsi-math-symbols-input.el〕 (if the file is not already that name).
;; ② Place the file in the dir 〔~/.emacs.d/〕. On Windows, it's 〔$HOMEPATH\.emacs.d\〕. Create the 〔.emacs.d〕 folder if you don't have it.
;; ③ Put the following lines in your emacs init file “.emacs”:
;; (add-to-list 'load-path "~/.emacs.d/")
;; (autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math/Unicode symbols." t)
;; (xmsi-mode 1) ; activate the mode.

;; Then, restart emacs.

;;; DOCUMENTATION

;; Type “inf”, then press 【Shift+Space】, then it becomes “∞”.
;; Other examples:
;; “a” ⇒ “α”.
;; “p” ⇒ “π”.
;; “!=” ⇒ “≠”.
;; “>=” ⇒ “≥”.
;; “=>” ⇒ “⇒”.
;; “->” ⇒ “→”.
;; “and” ⇒ “∧”.
;; etc.

;; For full list, call “xmsi-list-math-symbols”.

;; The abbreviations are based on Mathematica's aliases 【Esc abbrv Esc】 and SGML/HTML/XML char entity abbreviations.

;; Full documentation is at: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; To see the inline documentation, call “describe-function”, then type “xmsi-mode”.
;; (if you have not load the mode yet, first load it by calling “xmsi-mode”.)

;; Donation of $3 is appreciated. Paypal to 〔xah@xahlee.org〕

;;; HISTORY

;; v1.5.1, 2013-04-09 • added ㎝ ㎠ ㎤. Why not?
;; v1.4.9, 2013-03-09 • added abbrev “tv” for 「📺」.
;; v1.4.8, 2013-03-01 • added abbrev “esc” for 「⎋」. • removed “power” for 「⎋」 because it's incorrect. there's no dedicate symbol in unicode for this. • added “sleep” for 「☾」 • added “break” for 「⎊」 • added “pause” for 「⎉」 • removed abbrev “control” for 「✲」, added abbrev “ctrl” for 「✲」. Because that symbol is associated with keyboard label only, not really “control”. • added the cycle 「✲ ⎈ ‸」 for control key symbol. • changed “enter” to insert 「⌤」 instead of 「↵」, because the new is more correct. • added abbrev “helm” for 「⎈」, may use for Control key. Microsoft keyboard uses 「✲」. • added cycles 「⏎ ↩ ↵ ⌤ ⎆」 • added a cycle for undo 「↶ ⎌」 (the latter is proper undo symbol, but there's no corresponding symbol for redo). • added “alt” for 「⎇」
;; v1.4.7, 2013-01-21 • changed the name of abbrev for 「↖」 from “nwarr” to “home”. Similarly, 「↘」 from “searr” to “end”. • Added a cycle for left right delete ⌫ ⌦. • added “control” for 「✲」, used on Microsoft keyboards.
;; v1.4.6, 2013-01-15 • added abbrev “smiley” for “☺”, added abbrev “sad” for “☹”
;; v1.4.5, 2013-01-14 • added abbrev “wdash” for “〜” WAVE DASH. • removed abbrev “-” for MINUS SIGN because it's seldom used and can be confusing. It already has abbrev “minus”. • removed abbrev “o/”. Use html entity “Oslash” Ø or “oslash” ø or math “empty” or “es” for empty set ∅.
;; v1.4.4, 2013-01-13 • Fixed union and intersection. Fixed abbrev “menu” for the menu key symbol. Fixed empty set symbol. Other misc improvements.
;; v1.4.3, 2013-01-13 • major code tweak. Several new symbols are added. Abbrevs are cleaned up for the better. Cycle symbol implementation changed. Some abbrev are taken off. e.g. there was {circle ●} and {circle2 ○}, now just “circle”, and the black and white versions are a cycle.
;; v1.4.2, 2013-01-13 • added � into the cycle ? ？ �. Added cycle between black and white versions of triangle, heart, diamond, circle, square, etc.
;; v1.4.1, 2013-01-04 • added symbol for meter squared (m2 → ㎡). • Fixed xml entity abbrevs {lang, rang} to the correct angle brackets 〈〉. • major overhaul of braket abbrevs, now more intuitive. Abbrevs include: "" <> <<>> () [] [[]] [()] and more. Call xmsi-list-math-symbols for a list.
;; v1.4.0, 2012-12-24 • added cycle between several punctuations and the fullwidth version: {, . : ; ! ? &}
;; v1.3.9, 2012-12-15 • added cycle for SPACE 「 」 → NO-BREAK SPACE 「 」 → IDEOGRAPHIC SPACE 「　」. Also, FULLWIDTH COMMA 「，」 and comma are now cycle.
;; v1.3.8, 2012-12-09 • added cycle for 「· ．。」. That is, one of these cycles to other. Also, 「,→，」.
;; v1.3.7, 2012-06-28 • much improved parsing the input string. Now, if there's no text selection, then grab string from cursor point to the left up to a whitespace char (limit to 10 chars max), then try that, if not found, try with one char minus. e.g. If text is 「some abc▮」, try 「abc」, then 「bc」, then 「c」. This way, user doesn't have to add a whitespace as separator before the abbrev, or having to make a text selection. e.g. in coding elisp, if current text is 「(a▮」, user can call xmsi-change-to-symbol directly to get 「(α▮」.
;; v1.3.6, 2012-05-08 • fixed a bug when any abbrev involving tilde ~ won't work.
;; v1.3.5.4, 2012-04-04 • stopped printing a message when “xmsi-change-to-symbol” is called. Minor improvement to inline doc of “xmsi-change-to-symbol”.
;; v1.3.5.3, 2012-04-02 • changed “-” to from “‒” FIGURE DASH to “−” MINUS SIGN.
;; v1.3.5.2, 2012-04-01 • added ~ for ≈
;; v1.3.5, 2012-02-26 • fixed a bug with 「:)」 in v1.3.4. • added ≔ and ≕.
;; v1.3.4, 2012-02-22 • The input now won't take “(” as part when calling “xmsi-change-to-symbol”. This means, when coding lisp, if you have 「(a▮」 then after calling ““xmsi-change-to-symbol” it becomes 「(α▮」. • removed “(c)” for “©”, use “copy” instead.
;; v1.3.3, 2011-11-08 • much improved handling of getting current abbrev. This means, it works in minibuffer. For example, type 【M-x】 then type 【a】, then 【Shift+Space】, then that “a” becomes “α”. Before, it was a error.
;; v1.3.2, 2011-11-05 • fixed a major bug in v1.3.1. When no valid input are found, xmsi-list-math-symbols is now automatically called, but isn't. • Added about 5 more subscript symbols, e.g _h _k _l etc.
;; v1.3.1, 2011-11-05 • fixed a showstopper bug. in v1.3.0. support of input using unicode names. Before, it always just insert greek alpha char.
;; v1.3.0, 2011-11-04 • Added support of input using unicode names. For example: 「greek small letter alpha」 • Added support of input using decimal forms, for examples: {「955」, 「#955」} or hexadimal forms {「x3bb」, 「#x3bb」}. XML entities forms such as {「&#955;」, 「&#x3bb;」, 「alpha」} are still supported. • When no valid input are found, xmsi-list-math-symbols is now automatically called.
;; v1.2.13, 2011-10-28 Corrected a abbrev for Greek lowercase rho “ρ” with abbrev “r”.
;; v1.2.12, 2011-06-26 Fixed nbsp. It was inserting normal space. Now it's non-breaking space.
;; v1.2.11, 2011-06-24 Added about 20 white/black shapes. e.g. square, circle, star, club …. Some are have existing abbrev but now with more intuitive name. Also a couple other changes.
;; v1.2.10, 2011-06-10 Added/Changed “dell”, “delr”, “tabl”, “tabr”, “_”, “!?”, “?!”, “!!”.
;; v1.2.9, 2011-03-24 Added about 102 full width version chars. Example, “fw&” becomes “＆”. Abbrev all start with “fw”. Remove about 5 redundant abbrevs.
;; v1.2.8, 2011-02-12 Added ⟵ ⟶ ⟷ ⇔ ⟸ ⟹ ⟺ ↚ ↛ ↮ ⇍ ⇏ ⇎. (few other chars may have been added but missed being mentioned here.)
;; v1.2.6, 2011-02-11 emdash, endash, figure dash, now have keys m- n- f-. Also, removed some full words keys: “degree” (deg), “pi” (p), “infinity” (inf), “angle” (ang).
;; v1.2.5, 2011-02-01 added a figure dash.
;; v1.2.4, 2011-01-11 made the error message more clear. • added symbol for circled number ① ② ... ⒈⒉ ... 🄂 🄃 ...
;; v1.2.3, 2011-01-06 More clean up, adding symbols. The input string is changed. Now, it's from the cursor position to the left whitespace. Before this change, the input is determined by whitespace chars to the left and right.
;; v1.2.2, 2010-12-22 Added nbsp for non-breaking-space and some others symbols.
;; v1.2.1, 2010-12-14 Added about 10 more symbols.
;; v1.2, 2010-12-14 Added support to enter char by unicode decimal or hexadecimal.
;; v1.1, 2010-12-12 added more symbols.
;; v1.0, 2010-12-08 First version.

;;; TODO
;; • make the activation key customizable
;; • make it customizable to change/add abbrevs/symbols

;;; References
;; 〈Math Symbols in Unicode〉 http://xahlee.info/comp/unicode_math_operators.html
;; 〈HTML/XML Entities (Character/Unicode/Symbol) List〉 http://xahlee.info/comp/unicode_html_entities.html
;; 〈Math Font, Unicode, Gothic Letters, Double Struck〉 http://xahlee.info/math/math_font_unicode.html
;; 〈How Mathematica does Unicode?〉 http://xahlee.info/math/mathematica_unicode.html
;; http://ia.wikipedia.org/wiki/Wikipedia:LaTeX_symbols
;; http://en.wikipedia.org/wiki/Help:Displaying_a_formula
;; http://www.ctan.org/tex-archive/info/symbols/comprehensive/symbols-a4.pdf
;; http://en.wikipedia.org/wiki/Mathematical_alphanumeric_symbols
;; http://en.wikipedia.org/wiki/Astronomical_symbol
;; http://en.wikipedia.org/wiki/Double_struck



;;; Code:

(setq xmsi-version "v1.4.8")

(defvar xmsi-abrvs nil "A abbreviation hash table that maps a string to unicode char.")

(progn
  (setq xmsi-abrvs (make-hash-table :test 'equal))

  (progn
    ;; sgml/html/xhtml/xml entities
    (puthash "nbsp" " " xmsi-abrvs)
    (puthash "bull" "•" xmsi-abrvs)
    (puthash "iexcl" "¡" xmsi-abrvs)
    (puthash "cent" "¢" xmsi-abrvs)
    (puthash "pound" "£" xmsi-abrvs)
    (puthash "curren" "¤" xmsi-abrvs)
    (puthash "yen" "¥" xmsi-abrvs)
    (puthash "brvbar" "¦" xmsi-abrvs)
    (puthash "sect" "§" xmsi-abrvs)
    (puthash "uml" "¨" xmsi-abrvs)
    (puthash "copy" "©" xmsi-abrvs)
    (puthash "ordf" "ª" xmsi-abrvs)
    (puthash "laquo" "«" xmsi-abrvs)
    (puthash "not" "¬" xmsi-abrvs)
    (puthash "reg" "®" xmsi-abrvs)
    (puthash "macr" "¯" xmsi-abrvs)
    (puthash "deg" "°" xmsi-abrvs)
    (puthash "plusmn" "±" xmsi-abrvs)
    (puthash "sup2" "²" xmsi-abrvs)
    (puthash "sup3" "³" xmsi-abrvs)
    (puthash "acute" "´" xmsi-abrvs)
    (puthash "micro" "µ" xmsi-abrvs)
    (puthash "para" "¶" xmsi-abrvs)
    (puthash "middot" "·" xmsi-abrvs)
    (puthash "cedil" "¸" xmsi-abrvs)
    (puthash "sup1" "¹" xmsi-abrvs)
    (puthash "ordm" "º" xmsi-abrvs)
    (puthash "raquo" "»" xmsi-abrvs)
    (puthash "frac14" "¼" xmsi-abrvs)
    (puthash "frac12" "½" xmsi-abrvs)
    (puthash "frac34" "¾" xmsi-abrvs)
    (puthash "iquest" "¿" xmsi-abrvs)
    (puthash "Agrave" "À" xmsi-abrvs)
    (puthash "Aacute" "Á" xmsi-abrvs)
    (puthash "Acirc" "Â" xmsi-abrvs)
    (puthash "Atilde" "Ã" xmsi-abrvs)
    (puthash "Auml" "Ä" xmsi-abrvs)
    (puthash "Aring" "Å" xmsi-abrvs)
    (puthash "AElig" "Æ" xmsi-abrvs)
    (puthash "Ccedil" "Ç" xmsi-abrvs)
    (puthash "Egrave" "È" xmsi-abrvs)
    (puthash "Eacute" "É" xmsi-abrvs)
    (puthash "Ecirc" "Ê" xmsi-abrvs)
    (puthash "Euml" "Ë" xmsi-abrvs)
    (puthash "Igrave" "Ì" xmsi-abrvs)
    (puthash "Iacute" "Í" xmsi-abrvs)
    (puthash "Icirc" "Î" xmsi-abrvs)
    (puthash "Iuml" "Ï" xmsi-abrvs)
    (puthash "ETH" "Ð" xmsi-abrvs)
    (puthash "Ntilde" "Ñ" xmsi-abrvs)
    (puthash "Ograve" "Ò" xmsi-abrvs)
    (puthash "Oacute" "Ó" xmsi-abrvs)
    (puthash "Ocirc" "Ô" xmsi-abrvs)
    (puthash "Otilde" "Õ" xmsi-abrvs)
    (puthash "Ouml" "Ö" xmsi-abrvs)
    (puthash "times" "×" xmsi-abrvs)
    (puthash "Oslash" "Ø" xmsi-abrvs)
    (puthash "Ugrave" "Ù" xmsi-abrvs)
    (puthash "Uacute" "Ú" xmsi-abrvs)
    (puthash "Ucirc" "Û" xmsi-abrvs)
    (puthash "Uuml" "Ü" xmsi-abrvs)
    (puthash "Yacute" "Ý" xmsi-abrvs)
    (puthash "THORN" "Þ" xmsi-abrvs)
    (puthash "szlig" "ß" xmsi-abrvs)
    (puthash "agrave" "à" xmsi-abrvs)
    (puthash "aacute" "á" xmsi-abrvs)
    (puthash "acirc" "â" xmsi-abrvs)
    (puthash "atilde" "ã" xmsi-abrvs)
    (puthash "auml" "ä" xmsi-abrvs)
    (puthash "aring" "å" xmsi-abrvs)
    (puthash "aelig" "æ" xmsi-abrvs)
    (puthash "ccedil" "ç" xmsi-abrvs)
    (puthash "egrave" "è" xmsi-abrvs)
    (puthash "eacute" "é" xmsi-abrvs)
    (puthash "ecirc" "ê" xmsi-abrvs)
    (puthash "euml" "ë" xmsi-abrvs)
    (puthash "igrave" "ì" xmsi-abrvs)
    (puthash "iacute" "í" xmsi-abrvs)
    (puthash "icirc" "î" xmsi-abrvs)
    (puthash "iuml" "ï" xmsi-abrvs)
    (puthash "eth" "ð" xmsi-abrvs)
    (puthash "ntilde" "ñ" xmsi-abrvs)
    (puthash "ograve" "ò" xmsi-abrvs)
    (puthash "oacute" "ó" xmsi-abrvs)
    (puthash "ocirc" "ô" xmsi-abrvs)
    (puthash "otilde" "õ" xmsi-abrvs)
    (puthash "ouml" "ö" xmsi-abrvs)
    (puthash "divide" "÷" xmsi-abrvs)
    (puthash "oslash" "ø" xmsi-abrvs)
    (puthash "ugrave" "ù" xmsi-abrvs)
    (puthash "uacute" "ú" xmsi-abrvs)
    (puthash "ucirc" "û" xmsi-abrvs)
    (puthash "uuml" "ü" xmsi-abrvs)
    (puthash "yacute" "ý" xmsi-abrvs)
    (puthash "thorn" "þ" xmsi-abrvs)
    (puthash "yuml" "ÿ" xmsi-abrvs)
    (puthash "OElig" "Œ" xmsi-abrvs)
    (puthash "oelig" "œ" xmsi-abrvs)
    (puthash "Scaron" "Š" xmsi-abrvs)
    (puthash "scaron" "š" xmsi-abrvs)
    (puthash "Yuml" "Ÿ" xmsi-abrvs)
    (puthash "fnof" "ƒ" xmsi-abrvs)
    (puthash "circ" "ˆ" xmsi-abrvs)
    (puthash "tilde" "˜" xmsi-abrvs)
    (puthash "Alpha" "Α" xmsi-abrvs)
    (puthash "Beta" "Β" xmsi-abrvs)
    (puthash "Gamma" "Γ" xmsi-abrvs)
    (puthash "Delta" "Δ" xmsi-abrvs)
    (puthash "Epsilon" "Ε" xmsi-abrvs)
    (puthash "Zeta" "Ζ" xmsi-abrvs)
    (puthash "Eta" "Η" xmsi-abrvs)
    (puthash "Theta" "Θ" xmsi-abrvs)
    (puthash "Iota" "Ι" xmsi-abrvs)
    (puthash "Kappa" "Κ" xmsi-abrvs)
    (puthash "Lambda" "Λ" xmsi-abrvs)
    (puthash "Mu" "Μ" xmsi-abrvs)
    (puthash "Nu" "Ν" xmsi-abrvs)
    (puthash "Xi" "Ξ" xmsi-abrvs)
    (puthash "Omicron" "Ο" xmsi-abrvs)
    (puthash "Pi" "Π" xmsi-abrvs)
    (puthash "Rho" "Ρ" xmsi-abrvs)
    (puthash "Sigma" "Σ" xmsi-abrvs)
    (puthash "Tau" "Τ" xmsi-abrvs)
    (puthash "Upsilon" "Υ" xmsi-abrvs)
    (puthash "Phi" "Φ" xmsi-abrvs)
    (puthash "Chi" "Χ" xmsi-abrvs)
    (puthash "Psi" "Ψ" xmsi-abrvs)
    (puthash "Omega" "Ω" xmsi-abrvs)
    (puthash "alpha" "α" xmsi-abrvs)
    (puthash "beta" "β" xmsi-abrvs)
    (puthash "gamma" "γ" xmsi-abrvs)
    (puthash "delta" "δ" xmsi-abrvs)
    (puthash "epsilon" "ε" xmsi-abrvs)
    (puthash "zeta" "ζ" xmsi-abrvs)
    (puthash "eta" "η" xmsi-abrvs)
    (puthash "theta" "θ" xmsi-abrvs)
    (puthash "iota" "ι" xmsi-abrvs)
    (puthash "kappa" "κ" xmsi-abrvs)
    (puthash "lambda" "λ" xmsi-abrvs)
    (puthash "mu" "μ" xmsi-abrvs)
    (puthash "nu" "ν" xmsi-abrvs)
    (puthash "xi" "ξ" xmsi-abrvs)
    (puthash "omicron" "ο" xmsi-abrvs)
    (puthash "pi" "π" xmsi-abrvs)
    (puthash "rho" "ρ" xmsi-abrvs)
    (puthash "sigmaf" "ς" xmsi-abrvs)
    (puthash "sigma" "σ" xmsi-abrvs)
    (puthash "tau" "τ" xmsi-abrvs)
    (puthash "upsilon" "υ" xmsi-abrvs)
    (puthash "phi" "φ" xmsi-abrvs)
    (puthash "chi" "χ" xmsi-abrvs)
    (puthash "psi" "ψ" xmsi-abrvs)
    (puthash "omega" "ω" xmsi-abrvs)
    (puthash "thetasym" "ϑ" xmsi-abrvs)
    (puthash "upsih" "ϒ" xmsi-abrvs)
    (puthash "piv" "ϖ" xmsi-abrvs)
    (puthash "ndash" "–" xmsi-abrvs)
    (puthash "mdash" "—" xmsi-abrvs)
    (puthash "lsquo" "‘" xmsi-abrvs)
    (puthash "rsquo" "’" xmsi-abrvs)
    (puthash "sbquo" "‚" xmsi-abrvs)
    (puthash "ldquo" "“" xmsi-abrvs)
    (puthash "rdquo" "”" xmsi-abrvs)
    (puthash "bdquo" "„" xmsi-abrvs)
    (puthash "dagger" "†" xmsi-abrvs)
    (puthash "Dagger" "‡" xmsi-abrvs)
    (puthash "hellip" "…" xmsi-abrvs)
    (puthash "permil" "‰" xmsi-abrvs)
    (puthash "prime" "′" xmsi-abrvs)
    (puthash "Prime" "″" xmsi-abrvs)
    (puthash "lsaquo" "‹" xmsi-abrvs)
    (puthash "rsaquo" "›" xmsi-abrvs)
    (puthash "oline" "‾" xmsi-abrvs)
    (puthash "frasl" "⁄" xmsi-abrvs)
    (puthash "euro" "€" xmsi-abrvs)
    (puthash "image" "ℑ" xmsi-abrvs)
    (puthash "weierp" "℘" xmsi-abrvs)
    (puthash "real" "ℜ" xmsi-abrvs)
    (puthash "trade" "™" xmsi-abrvs)
    (puthash "alefsym" "ℵ" xmsi-abrvs)
    (puthash "larr" "←" xmsi-abrvs)
    (puthash "uarr" "↑" xmsi-abrvs)
    (puthash "rarr" "→" xmsi-abrvs)
    (puthash "darr" "↓" xmsi-abrvs)
    (puthash "harr" "↔" xmsi-abrvs)
    (puthash "crarr" "↵" xmsi-abrvs)
    (puthash "lArr" "⇐" xmsi-abrvs)
    (puthash "uArr" "⇑" xmsi-abrvs)
    (puthash "rArr" "⇒" xmsi-abrvs)
    (puthash "dArr" "⇓" xmsi-abrvs)
    (puthash "hArr" "⇔" xmsi-abrvs)
    (puthash "forall" "∀" xmsi-abrvs)
    (puthash "part" "∂" xmsi-abrvs)
    (puthash "exist" "∃" xmsi-abrvs)
    (puthash "nabla" "∇" xmsi-abrvs)
    (puthash "isin" "∈" xmsi-abrvs)
    (puthash "notin" "∉" xmsi-abrvs)
    (puthash "ni" "∋" xmsi-abrvs)
    (puthash "prod" "∏" xmsi-abrvs)
    (puthash "sum" "∑" xmsi-abrvs)
    (puthash "minus" "−" xmsi-abrvs)
    (puthash "lowast" "∗" xmsi-abrvs)
    (puthash "radic" "√" xmsi-abrvs)
    (puthash "prop" "∝" xmsi-abrvs)
    (puthash "infin" "∞" xmsi-abrvs)
    (puthash "ang" "∠" xmsi-abrvs)
    (puthash "and" "∧" xmsi-abrvs)
    (puthash "or" "∨" xmsi-abrvs)
    (puthash "cap" "∩" xmsi-abrvs)
    (puthash "cup" "∪" xmsi-abrvs)
    (puthash "int" "∫" xmsi-abrvs)
    (puthash "there4" "∴" xmsi-abrvs)
    (puthash "sim" "∼" xmsi-abrvs)
    (puthash "cong" "≅" xmsi-abrvs)
    (puthash "asymp" "≈" xmsi-abrvs)
    (puthash "ne" "≠" xmsi-abrvs)
    (puthash "equiv" "≡" xmsi-abrvs)
    (puthash "le" "≤" xmsi-abrvs)
    (puthash "ge" "≥" xmsi-abrvs)
    (puthash "sub" "⊂" xmsi-abrvs)
    (puthash "sup" "⊃" xmsi-abrvs)
    (puthash "nsub" "⊄" xmsi-abrvs)
    (puthash "sube" "⊆" xmsi-abrvs)
    (puthash "supe" "⊇" xmsi-abrvs)
    (puthash "oplus" "⊕" xmsi-abrvs)
    (puthash "otimes" "⊗" xmsi-abrvs)
    (puthash "perp" "⊥" xmsi-abrvs)
    (puthash "sdot" "⋅" xmsi-abrvs)
    (puthash "lceil" "⌈" xmsi-abrvs)
    (puthash "rceil" "⌉" xmsi-abrvs)
    (puthash "lfloor" "⌊" xmsi-abrvs)
    (puthash "rfloor" "⌋" xmsi-abrvs)
    (puthash "lang" "〈" xmsi-abrvs)
    (puthash "rang" "〉" xmsi-abrvs)
    (puthash "loz" "◊" xmsi-abrvs)
    (puthash "spades" "♠" xmsi-abrvs)
    (puthash "clubs" "♣" xmsi-abrvs)
    (puthash "hearts" "♥" xmsi-abrvs)
    (puthash "diams" "♦" xmsi-abrvs)
    )

(progn
  ;; Double struck letter forms (aka Double struck; double stroke) Others are outside of the BMP (Unicode's Basic Multilingual Plane). Almost no font supports it.
  (puthash "dsC" "ℂ" xmsi-abrvs)
  (puthash "dsH" "ℍ" xmsi-abrvs)
  (puthash "dsN" "ℕ" xmsi-abrvs)
  (puthash "dsP" "ℙ" xmsi-abrvs)
  (puthash "dsQ" "ℚ" xmsi-abrvs)
  (puthash "dsR" "ℝ" xmsi-abrvs)
  (puthash "dsZ" "ℤ" xmsi-abrvs)

  (puthash "dd" "ⅆ" xmsi-abrvs)
  (puthash "ee" "ⅇ" xmsi-abrvs)
  (puthash "ii" "ⅈ" xmsi-abrvs)
  (puthash "jj" "ⅉ" xmsi-abrvs)

  (puthash "dsd" "ⅆ" xmsi-abrvs)
  (puthash "dse" "ⅇ" xmsi-abrvs)
  (puthash "dsi" "ⅈ" xmsi-abrvs)
  (puthash "dsj" "ⅉ" xmsi-abrvs)
)

(progn
  ;; gothic letter forms (aka FRANKTUR). Most are outside BMP
  (puthash "goA" "𝔄" xmsi-abrvs)
  (puthash "goB" "𝔅" xmsi-abrvs)
  (puthash "goC" "ℭ" xmsi-abrvs)
  (puthash "goD" "𝔇" xmsi-abrvs)
  (puthash "goE" "𝔈" xmsi-abrvs)
  (puthash "goF" "𝔉" xmsi-abrvs)
  (puthash "goG" "𝔊" xmsi-abrvs)
  (puthash "goH" "ℌ" xmsi-abrvs)
  (puthash "goI" "ℑ" xmsi-abrvs)
  (puthash "goJ" "𝔍" xmsi-abrvs)
  (puthash "goK" "𝔎" xmsi-abrvs)
  (puthash "goL" "𝔏" xmsi-abrvs)
  (puthash "goM" "𝔐" xmsi-abrvs)
  (puthash "goN" "𝔑" xmsi-abrvs)
  (puthash "goO" "𝔒" xmsi-abrvs)
  (puthash "goP" "𝔓" xmsi-abrvs)
  (puthash "goQ" "𝔔" xmsi-abrvs)
  (puthash "goR" "ℜ" xmsi-abrvs)
  (puthash "goS" "𝔖" xmsi-abrvs)
  (puthash "goT" "𝔗" xmsi-abrvs)
  (puthash "goU" "𝔘" xmsi-abrvs)
  (puthash "goV" "𝔙" xmsi-abrvs)
  (puthash "goW" "𝔚" xmsi-abrvs)
  (puthash "goX" "𝔛" xmsi-abrvs)
  (puthash "goY" "𝔜" xmsi-abrvs)
  (puthash "goZ" "ℨ" xmsi-abrvs)
  (puthash "goa" "𝔞" xmsi-abrvs)
  (puthash "gob" "𝔟" xmsi-abrvs)
  (puthash "goc" "𝔠" xmsi-abrvs)
  (puthash "god" "𝔡" xmsi-abrvs)
  (puthash "goe" "𝔢" xmsi-abrvs)
  (puthash "gof" "𝔣" xmsi-abrvs)
  (puthash "gog" "𝔤" xmsi-abrvs)
  (puthash "goh" "𝔥" xmsi-abrvs)
  (puthash "goi" "𝔦" xmsi-abrvs)
  (puthash "goj" "𝔧" xmsi-abrvs)
  (puthash "gok" "𝔨" xmsi-abrvs)
  (puthash "gol" "𝔩" xmsi-abrvs)
  (puthash "gom" "𝔪" xmsi-abrvs)
  (puthash "gon" "𝔫" xmsi-abrvs)
  (puthash "goo" "𝔬" xmsi-abrvs)
  (puthash "gop" "𝔭" xmsi-abrvs)
  (puthash "goq" "𝔮" xmsi-abrvs)
  (puthash "gor" "𝔯" xmsi-abrvs)
  (puthash "gos" "𝔰" xmsi-abrvs)
  (puthash "got" "𝔱" xmsi-abrvs)
  (puthash "gou" "𝔲" xmsi-abrvs)
  (puthash "gov" "𝔳" xmsi-abrvs)
  (puthash "gow" "𝔴" xmsi-abrvs)
  (puthash "gox" "𝔵" xmsi-abrvs)
  (puthash "goy" "𝔶" xmsi-abrvs)
  (puthash "goz" "𝔷" xmsi-abrvs)
)

(progn
  ;; Scripted letter forms. Most are outside BMP.
  (puthash "sca" "𝒶" xmsi-abrvs)
  (puthash "scb" "𝒷" xmsi-abrvs)
  (puthash "scc" "𝒸" xmsi-abrvs)
  (puthash "scd" "𝒹" xmsi-abrvs)
  (puthash "sce" "ℯ" xmsi-abrvs) ; in BMP
  (puthash "scf" "𝒻" xmsi-abrvs)
  (puthash "scg" "ℊ" xmsi-abrvs) ; in BMP
  (puthash "sch" "𝒽" xmsi-abrvs)
  (puthash "sci" "𝒾" xmsi-abrvs)
  (puthash "scj" "𝒿" xmsi-abrvs)
  (puthash "sck" "𝓀" xmsi-abrvs)        ;
  (puthash "scl2" "𝓁" xmsi-abrvs)       ;MATHEMATICAL SCRIPT SMALL L
  (puthash "scl" "ℓ" xmsi-abrvs)        ;in BMP ; SCRIPT SMALL L
  (puthash "scm" "𝓂" xmsi-abrvs)
  (puthash "scn" "𝓃" xmsi-abrvs)
  (puthash "sco" "ℴ" xmsi-abrvs) ; in BMP ;SCRIPT SMALL O
  (puthash "scp" "𝓅" xmsi-abrvs)
  (puthash "scq" "𝓆" xmsi-abrvs)
  (puthash "scw" "𝓌" xmsi-abrvs)
  (puthash "scx" "𝓍" xmsi-abrvs)
  (puthash "scy" "𝓎" xmsi-abrvs)
  (puthash "scz" "𝓏" xmsi-abrvs)

  (puthash "scB" "ℬ" xmsi-abrvs)
  (puthash "scE" "ℰ" xmsi-abrvs)
  (puthash "scF" "ℱ" xmsi-abrvs)
  (puthash "scH" "ℋ" xmsi-abrvs)
  (puthash "scI" "ℐ" xmsi-abrvs)
  (puthash "scL" "ℒ" xmsi-abrvs)
  (puthash "scM" "ℳ" xmsi-abrvs)
  (puthash "scP" "℘" xmsi-abrvs)
  (puthash "scR" "ℛ" xmsi-abrvs)
)

 ;; a b c d e f g h i j k l m n o p q w x y z
 ;; A B C D E F G H I J K L M N O P Q W X Y Z

(progn
  ;; accented letters
  (puthash "a`" "à" xmsi-abrvs)
  (puthash "e`" "è" xmsi-abrvs)
  (puthash "i`" "ì" xmsi-abrvs)
  (puthash "o`" "ò" xmsi-abrvs)
  (puthash "u`" "ù" xmsi-abrvs)
  (puthash "A`" "À" xmsi-abrvs)
  (puthash "E`" "È" xmsi-abrvs)
  (puthash "I`" "Ì" xmsi-abrvs)
  (puthash "O`" "Ò" xmsi-abrvs)
  (puthash "U`" "Ù" xmsi-abrvs)

  (puthash "a^" "â" xmsi-abrvs)
  (puthash "e^" "ê" xmsi-abrvs)
  (puthash "i^" "î" xmsi-abrvs)
  (puthash "o^" "ô" xmsi-abrvs)
  (puthash "u^" "û" xmsi-abrvs)
  (puthash "A^" "Â" xmsi-abrvs)
  (puthash "E^" "Ê" xmsi-abrvs)
  (puthash "I^" "Î" xmsi-abrvs)
  (puthash "O^" "Ô" xmsi-abrvs)
  (puthash "U^" "Û" xmsi-abrvs)

  (puthash "a'" "á" xmsi-abrvs)
  (puthash "e'" "é" xmsi-abrvs)
  (puthash "i'" "í" xmsi-abrvs)
  (puthash "o'" "ó" xmsi-abrvs)
  (puthash "u'" "ú" xmsi-abrvs)
  (puthash "y'" "ý" xmsi-abrvs)
  (puthash "A'" "Á" xmsi-abrvs)
  (puthash "E'" "É" xmsi-abrvs)
  (puthash "I'" "Í" xmsi-abrvs)
  (puthash "O'" "Ó" xmsi-abrvs)
  (puthash "U'" "Ú" xmsi-abrvs)
  (puthash "Y'" "Ý" xmsi-abrvs)

  (puthash "A\"" "Ä" xmsi-abrvs)
  (puthash "E\"" "Ë" xmsi-abrvs)
  (puthash "I\"" "Ï" xmsi-abrvs)
  (puthash "O\"" "Ö" xmsi-abrvs)
  (puthash "U\"" "Ü" xmsi-abrvs)
  (puthash "a\"" "ä" xmsi-abrvs)
  (puthash "e\"" "ë" xmsi-abrvs)
  (puthash "i\"" "ï" xmsi-abrvs)
  (puthash "o\"" "ö" xmsi-abrvs)
  (puthash "u\"" "ü" xmsi-abrvs)
  (puthash "s\"" "ß" xmsi-abrvs)
  (puthash "y\"" "ÿ" xmsi-abrvs)

  (puthash "Ao" "Å" xmsi-abrvs)
  (puthash "ao" "å" xmsi-abrvs)

  (puthash "AE" "Æ" xmsi-abrvs)
  (puthash "ae" "æ" xmsi-abrvs)

  (puthash "a~" "ã" xmsi-abrvs)
  (puthash "n~" "ñ" xmsi-abrvs)
  (puthash "o~" "õ" xmsi-abrvs)
  (puthash "A~" "Ã" xmsi-abrvs)
  (puthash "N~" "Ñ" xmsi-abrvs)
  (puthash "O~" "Õ" xmsi-abrvs)
)

(progn
  ;; misc non-math symbols
  (puthash "tm" "™" xmsi-abrvs)
  (puthash "3/4" "¾" xmsi-abrvs)
  (puthash "1/2" "½" xmsi-abrvs)
  (puthash "1/4" "¼" xmsi-abrvs)
  (puthash "..." "…" xmsi-abrvs)        ;HORIZONTAL ELLIPSIS
  (puthash "fdash" "‒" xmsi-abrvs) ;FIGURE DASH. abbrev consistent with html entity mdash ndash
  (puthash "wdash" "〜" xmsi-abrvs) ; WAVE DASH
  (puthash "--" "—" xmsi-abrvs)     ;EM DASH
  (puthash "?!" "⁈" xmsi-abrvs)
  (puthash "!?" "⁉" xmsi-abrvs)
  (puthash "!!" "‼" xmsi-abrvs)
  (puthash "m2" "㎡" xmsi-abrvs)        ;SQUARE M SQUARED , meter squared
(puthash "cm" "㎝" xmsi-abrvs) ; centimeter
(puthash "cm2" "㎠" xmsi-abrvs)
(puthash "cm3" "㎤" xmsi-abrvs)

  (puthash "smiley" "☺" xmsi-abrvs)     ;WHITE SMILING FACE smiley, happy face
  (puthash ":)" "☺" xmsi-abrvs)
  (puthash ":(" "☹" xmsi-abrvs)
  (puthash "sad" "☹" xmsi-abrvs)        ; WHITE FROWNING FACE
  )

(progn
  ;; computer keys and symbols
  (puthash "cmd" "⌘" xmsi-abrvs)
  (puthash "opt" "⌥" xmsi-abrvs)        ; OPTION KEY
  (puthash "alt" "⎇" xmsi-abrvs)        ; ALTERNATIVE KEY SYMBOL
  (puthash "ctrl" "✲" xmsi-abrvs) ; OPEN CENTRE ASTERISK used by Microsoft on their keyboards.
  (puthash "helm" "⎈" xmsi-abrvs) ; HELM SYMBOL, may use for control key.
  (puthash "caret" "‸" xmsi-abrvs)      ; CARET control key symbol.
  (puthash "menu" "▤" xmsi-abrvs)      ; SQUARE WITH HORIZONTAL FILL for menu key.

  (puthash "enter" "⌤" xmsi-abrvs)
  (puthash "return" "⏎" xmsi-abrvs)
  (puthash "pgup" "⇞" xmsi-abrvs)
  (puthash "pgdn" "⇟" xmsi-abrvs)
  (puthash "home" "↖" xmsi-abrvs)
  (puthash "end" "↘" xmsi-abrvs)
  (puthash "esc" "⎋" xmsi-abrvs)        ; used in Apple's doc and GUI menu

  (puthash "eject" "⏏" xmsi-abrvs)
  (puthash "undo" "↶" xmsi-abrvs) ; more proper is ⎌, but there's no corresponding redo.
  (puthash "redo" "↷" xmsi-abrvs)
  (puthash "shift" "⇧" xmsi-abrvs)

  (puthash "delete" "⌫" xmsi-abrvs)
  (puthash "dell" "⌫" xmsi-abrvs)
  (puthash "delr" "⌦" xmsi-abrvs)
  (puthash "space" "␣" xmsi-abrvs)      ; OPEN BOX
  (puthash "lrarr" "⇄" xmsi-abrvs)
  (puthash "|<-" "⇤" xmsi-abrvs)
  (puthash "->|" "⇥" xmsi-abrvs)
  (puthash "tabl" "⇤" xmsi-abrvs)
  (puthash "tabr" "⇥" xmsi-abrvs)
  (puthash "tab" "↹" xmsi-abrvs)

  (puthash "sleep" "☾" xmsi-abrvs)  ; LAST QUARTER MOON. for Sleep key
  (puthash "break" "⎊" xmsi-abrvs)  ; CIRCLED TRIANGLE DOWN for Break key
  (puthash "pause" "⎉" xmsi-abrvs)  ; CIRCLED HORIZONTAL BAR WITH NOTCH for Pause key
  (puthash "prevpage" "⎗" xmsi-abrvs)
  (puthash "nextpage" "⎘" xmsi-abrvs)
  (puthash "print" "⎙" xmsi-abrvs)

  (puthash "keyboard" "⌨" xmsi-abrvs)

  (puthash "clear" "⌧" xmsi-abrvs)
  (puthash "cursor" "▮" xmsi-abrvs)
  (puthash "ibeam" "⌶" xmsi-abrvs)
  (puthash "watch" "⌚" xmsi-abrvs)
  (puthash "hourglass" "⌛" xmsi-abrvs)
  (puthash "scissor" "✂" xmsi-abrvs)    ;BLACK SCISSORS
  (puthash "envelope" "✉" xmsi-abrvs)
  (puthash "writing" "✍" xmsi-abrvs)
)

(progn
  ;; superscripts
  (puthash "^0" "⁰" xmsi-abrvs)
  (puthash "^1" "¹" xmsi-abrvs)
  (puthash "^2" "²" xmsi-abrvs)
  (puthash "^3" "³" xmsi-abrvs)
  (puthash "^4" "⁴" xmsi-abrvs)
  (puthash "^5" "⁵" xmsi-abrvs)
  (puthash "^6" "⁶" xmsi-abrvs)
  (puthash "^7" "⁷" xmsi-abrvs)
  (puthash "^8" "⁸" xmsi-abrvs)
  (puthash "^9" "⁹" xmsi-abrvs)
  (puthash "^+" "⁺" xmsi-abrvs)
  (puthash "^-" "⁻" xmsi-abrvs)
  (puthash "^=" "⁼" xmsi-abrvs)
  (puthash "^(" "⁽" xmsi-abrvs)
  (puthash "^)" "⁾" xmsi-abrvs)
  (puthash "^n" "ⁿ" xmsi-abrvs)
  (puthash "^i" "ⁱ" xmsi-abrvs)

  ;; subscripts
  (puthash "_(" "₍" xmsi-abrvs)
  (puthash "_)" "₎" xmsi-abrvs)
  (puthash "_+" "₊" xmsi-abrvs)
  (puthash "_-" "₋" xmsi-abrvs)
  (puthash "_0" "₀" xmsi-abrvs)
  (puthash "_1" "₁" xmsi-abrvs)
  (puthash "_2" "₂" xmsi-abrvs)
  (puthash "_3" "₃" xmsi-abrvs)
  (puthash "_4" "₄" xmsi-abrvs)
  (puthash "_5" "₅" xmsi-abrvs)
  (puthash "_6" "₆" xmsi-abrvs)
  (puthash "_7" "₇" xmsi-abrvs)
  (puthash "_8" "₈" xmsi-abrvs)
  (puthash "_9" "₉" xmsi-abrvs)
  (puthash "_=" "₌" xmsi-abrvs)
  (puthash "_a" "ₐ" xmsi-abrvs)
  (puthash "_e" "ₑ" xmsi-abrvs)

  (puthash "_h" "ₕ" xmsi-abrvs)
  (puthash "_i" "ᵢ" xmsi-abrvs)
  (puthash "_j" "ⱼ" xmsi-abrvs)
  (puthash "_k" "ₖ" xmsi-abrvs)
  (puthash "_l" "ₗ" xmsi-abrvs)
  (puthash "_m" "ₘ" xmsi-abrvs)
  (puthash "_n" "ₙ" xmsi-abrvs)
  (puthash "_o" "ₒ" xmsi-abrvs)
  (puthash "_p" "ₚ" xmsi-abrvs)
  (puthash "_r" "ᵣ" xmsi-abrvs)
  (puthash "_s" "ₛ" xmsi-abrvs)
  (puthash "_t" "ₜ" xmsi-abrvs)
  (puthash "_u" "ᵤ" xmsi-abrvs)
  (puthash "_v" "ᵥ" xmsi-abrvs)
  (puthash "_x" "ₓ" xmsi-abrvs)
  (puthash "_schwa" "ₔ" xmsi-abrvs)
)

(progn
  ;; astronomy
  (puthash "sun" "☉" xmsi-abrvs)
  (puthash "sunray" "☼" xmsi-abrvs)
  (puthash "moon" "☾" xmsi-abrvs)
  (puthash "moonr" "☽" xmsi-abrvs)
  (puthash "mercury" "☿" xmsi-abrvs)
  (puthash "earth" "♁" xmsi-abrvs)
  (puthash "saturn" "♄" xmsi-abrvs)
  (puthash "uranus" "♅" xmsi-abrvs)
  (puthash "neptune" "♆" xmsi-abrvs)
  (puthash "pluto" "♇" xmsi-abrvs)
  (puthash "jupiter" "♃" xmsi-abrvs)
  (puthash "male" "♂" xmsi-abrvs)
  (puthash "mars" "♂" xmsi-abrvs)
  (puthash "female" "♀" xmsi-abrvs)
  (puthash "venus" "♀" xmsi-abrvs)
  (puthash "comet" "☄" xmsi-abrvs)
)

(progn
  ;; forms for constants-like things
  (puthash "inf" "∞" xmsi-abrvs)        ;INFINITY
  (puthash "empty" "∅" xmsi-abrvs)         ;EMPTY SET
  (puthash "es" "∅" xmsi-abrvs)         ;EMPTY SET not to be confused with  Ø ø Oslash oslash
  ;; misc math
  (puthash "+-" "±" xmsi-abrvs)
  (puthash "-+" "∓" xmsi-abrvs)
)

(progn
  ;; brackets, matching pairs
  (puthash "flr" "⌊⌋" xmsi-abrvs)       ; floor
  (puthash "ceil" "⌈⌉" xmsi-abrvs)      ; ceiling
  (puthash "floor" "⌊⌋" xmsi-abrvs)       ; floor
  (puthash "ceiling" "⌈⌉" xmsi-abrvs)      ; ceiling

  (puthash "\"" "“”" xmsi-abrvs)        ;curly quote
  (puthash "\"\"" "“”" xmsi-abrvs)

  (puthash "<>" "‹›" xmsi-abrvs)        ;french quote
  (puthash "<<>>" "«»" xmsi-abrvs)

  (puthash "[" "「」" xmsi-abrvs)
  (puthash "[]" "「」" xmsi-abrvs)
  (puthash "[[" "『』" xmsi-abrvs)
  (puthash "[[]]" "『』" xmsi-abrvs)
  (puthash "[(" "【】" xmsi-abrvs)
  (puthash "[()]" "【】" xmsi-abrvs)
  (puthash "(" "〔〕" xmsi-abrvs)
  (puthash "()" "〔〕" xmsi-abrvs)
)

(progn
  ;; number forms
  (puthash "c1" "①" xmsi-abrvs)
  (puthash "c2" "②" xmsi-abrvs)
  (puthash "c3" "③" xmsi-abrvs)
  (puthash "c4" "④" xmsi-abrvs)
  (puthash "c5" "⑤" xmsi-abrvs)
  (puthash "c6" "⑥" xmsi-abrvs)
  (puthash "c7" "⑦" xmsi-abrvs)
  (puthash "c8" "⑧" xmsi-abrvs)
  (puthash "c9" "⑨" xmsi-abrvs)
  (puthash "c0" "⓪" xmsi-abrvs)

  (puthash "1." "⒈" xmsi-abrvs)
  (puthash "2." "⒉" xmsi-abrvs)
  (puthash "3." "⒊" xmsi-abrvs)
  (puthash "4." "⒋" xmsi-abrvs)
  (puthash "5." "⒌" xmsi-abrvs)
  (puthash "6." "⒍" xmsi-abrvs)
  (puthash "7." "⒎" xmsi-abrvs)
  (puthash "8." "⒏" xmsi-abrvs)
  (puthash "9." "⒐" xmsi-abrvs)
  (puthash "0." "🄀" xmsi-abrvs)

  (puthash "1," "🄂" xmsi-abrvs)
  (puthash "2," "🄃" xmsi-abrvs)
  (puthash "3," "🄄" xmsi-abrvs)
  (puthash "4," "🄅" xmsi-abrvs)
  (puthash "5," "🄆" xmsi-abrvs)
  (puthash "6," "🄇" xmsi-abrvs)
  (puthash "7," "🄈" xmsi-abrvs)
  (puthash "8," "🄉" xmsi-abrvs)
  (puthash "9," "🄊" xmsi-abrvs)
  (puthash "0," "🄁" xmsi-abrvs)
  )

  ;; music
(puthash "notes4" "♩" xmsi-abrvs)
(puthash "notes8" "♪" xmsi-abrvs)
(puthash "notes8d" "♫" xmsi-abrvs)
(puthash "notes16d" "♬" xmsi-abrvs)
(puthash "flat" "♭" xmsi-abrvs)
(puthash "natural" "♮" xmsi-abrvs)
(puthash "sharp" "♯" xmsi-abrvs)

(progn
  ;; letters
  ;; greek alphabets http://en.wikipedia.org/wiki/Greek_alphabet
  ;; ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ
  ;; αβγδεζηθικλμνξοπρστυφχψω
  ;;                  ς
  (puthash "a" "α" xmsi-abrvs)
  (puthash "b" "β" xmsi-abrvs)
  (puthash "g" "γ" xmsi-abrvs)
  (puthash "d" "δ" xmsi-abrvs)
  (puthash "e" "ε" xmsi-abrvs)
  (puthash "z" "ζ" xmsi-abrvs)
  (puthash "h" "η" xmsi-abrvs)
  (puthash "q" "θ" xmsi-abrvs)
  (puthash "i" "ι" xmsi-abrvs)
  (puthash "k" "κ" xmsi-abrvs)
  (puthash "l" "λ" xmsi-abrvs)
  (puthash "m" "μ" xmsi-abrvs)
  (puthash "n" "ν" xmsi-abrvs)
  (puthash "x" "ξ" xmsi-abrvs)
  ;; (puthash "a" "ο" xmsi-abrvs)
  (puthash "p" "π" xmsi-abrvs)
  (puthash "r" "ρ" xmsi-abrvs)
  (puthash "s" "σ" xmsi-abrvs)
  (puthash "t" "τ" xmsi-abrvs)
  (puthash "v" "υ" xmsi-abrvs)
  (puthash "f" "φ" xmsi-abrvs)
  (puthash "c" "χ" xmsi-abrvs)
  (puthash "y" "ψ" xmsi-abrvs)
  (puthash "o" "ω" xmsi-abrvs)

  (puthash "A" "Α" xmsi-abrvs)
  (puthash "B" "Β" xmsi-abrvs)
  (puthash "G" "Γ" xmsi-abrvs)
  (puthash "D" "Δ" xmsi-abrvs)
  (puthash "E" "Ε" xmsi-abrvs)
  (puthash "Z" "Ζ" xmsi-abrvs)
  (puthash "h" "Η" xmsi-abrvs)
  (puthash "Q" "Θ" xmsi-abrvs)
  (puthash "I" "Ι" xmsi-abrvs)
  (puthash "K" "Κ" xmsi-abrvs)
  (puthash "L" "Λ" xmsi-abrvs)
  (puthash "M" "Μ" xmsi-abrvs)
  (puthash "N" "Ν" xmsi-abrvs)
  (puthash "X" "Ξ" xmsi-abrvs)
  ;; (Puthash "A" "Ο" xmsi-abrvs)
  (puthash "P" "Π" xmsi-abrvs)
  (puthash "R" "Ρ" xmsi-abrvs)
  (puthash "S" "Σ" xmsi-abrvs)
  (puthash "T" "Τ" xmsi-abrvs)
  (puthash "V" "Υ" xmsi-abrvs)
  (puthash "F" "Φ" xmsi-abrvs)
  (puthash "C" "Χ" xmsi-abrvs)
  (puthash "Y" "Ψ" xmsi-abrvs)
  (puthash "O" "Ω" xmsi-abrvs)
)

(progn
  ;; letter-like forms
  (puthash "al" "ℵ" xmsi-abrvs)
  (puthash "alef" "ℵ" xmsi-abrvs)
  (puthash "beth" "ב" xmsi-abrvs)
  (puthash "gimel" "ג" xmsi-abrvs)
  (puthash "dalet" "ד" xmsi-abrvs)
  (puthash "daleth" "ד" xmsi-abrvs)
  (puthash "Digamma" "Ϝ" xmsi-abrvs)
  (puthash "digamma" "ϝ" xmsi-abrvs)
  (puthash "wp" "℘" xmsi-abrvs)
  (puthash "angstrom" "Å" xmsi-abrvs)
  (puthash "R2" "ℝ²" xmsi-abrvs)
  (puthash "R3" "ℝ³" xmsi-abrvs)
  (puthash "fun" "ƒ" xmsi-abrvs)
)

  ;; relations
  (puthash "<=" "≤" xmsi-abrvs)
  (puthash ">=" "≥" xmsi-abrvs)
  (puthash "!el" "∉" xmsi-abrvs)
  (puthash "el" "∈" xmsi-abrvs)
  (puthash "&&" "∧" xmsi-abrvs)
  (puthash "||" "∨" xmsi-abrvs)
  (puthash "not" "¬" xmsi-abrvs) ; not
  (puthash "===" "≡" xmsi-abrvs) ; equivalent

  (puthash "~" "≈" xmsi-abrvs) ; ALMOST EQUAL TO
  (puthash ":=" "≔" xmsi-abrvs) ; define
  (puthash "=:" "≕" xmsi-abrvs) ; define
  (puthash "!=" "≠" xmsi-abrvs) (puthash "notequal" "≠" xmsi-abrvs) ; not equal
  (puthash "fa" "∀" xmsi-abrvs) (puthash "forall" "∀" xmsi-abrvs) ; FOR ALL
  (puthash "ex" "∃" xmsi-abrvs) ; THERE EXISTS

(progn
  ;; operators
  (puthash "c+" "⊕" xmsi-abrvs)
  (puthash "c*" "⊗" xmsi-abrvs)
  (puthash "*" "×" xmsi-abrvs)
  (puthash "'" "′" xmsi-abrvs)
  (puthash "''" "″" xmsi-abrvs)
  (puthash "'''" "‴" xmsi-abrvs)
  (puthash "." "·" xmsi-abrvs)
  (puthash "root" "√" xmsi-abrvs)
  (puthash "sqrt" "√" xmsi-abrvs)
  (puthash "rt" "√" xmsi-abrvs)
  (puthash "del" "∇" xmsi-abrvs)
  (puthash "part" "∂" xmsi-abrvs)
  (puthash "partial" "∂" xmsi-abrvs)
  (puthash "pd" "∂" xmsi-abrvs)
  (puthash "cross" "⨯" xmsi-abrvs)
  (puthash "cint" "∮" xmsi-abrvs) ; contour integral
  (puthash "ccint" "∲" xmsi-abrvs)
  (puthash "cccint" "∳" xmsi-abrvs)
  (puthash "union" "∪" xmsi-abrvs)
  (puthash "intersection" "∩" xmsi-abrvs)
)

  (puthash "/_" "∠" xmsi-abrvs)         ;ANGLE
  (puthash "rightangle" "⦜" xmsi-abrvs)
  (puthash "|_" "⦜" xmsi-abrvs)
  (puthash "measuredangle" "∡" xmsi-abrvs)
  (puthash "sphericalangle" "∢" xmsi-abrvs)

  ;; arrows and maps
  (puthash "<-" "←" xmsi-abrvs)
  (puthash "->" "→" xmsi-abrvs)
  (puthash "<->" "↔" xmsi-abrvs)
  (puthash "!<-" "↚" xmsi-abrvs)
  (puthash "!->" "↛" xmsi-abrvs)
  (puthash "!<->" "↮" xmsi-abrvs)

(puthash "≤" "⇐" xmsi-abrvs)            ;LEFTWARDS DOUBLE ARROW

(puthash "=>" "⇒" xmsi-abrvs)
(puthash "<=>" "⇔" xmsi-abrvs)
(puthash "!<=" "⇍" xmsi-abrvs)
(puthash "!=>" "⇏" xmsi-abrvs)
(puthash "!=>" "⇎" xmsi-abrvs)

(puthash "<==" "⟸" xmsi-abrvs)
(puthash "==>" "⟹" xmsi-abrvs)          ;LONG RIGHTWARDS DOUBLE ARROW
(puthash "<==>" "⟺" xmsi-abrvs)

  (puthash "<-|" "↤" xmsi-abrvs)
  (puthash "|->" "↦" xmsi-abrvs)        ;RIGHTWARDS ARROW FROM BAR

  (puthash "<--" "⟵" xmsi-abrvs)
  (puthash "-->" "⟶" xmsi-abrvs)        ;LONG RIGHTWARDS ARROW
  (puthash "<-->" "⟷" xmsi-abrvs)

  (puthash "xor" "⊻" xmsi-abrvs)
  (puthash "nand" "⊼" xmsi-abrvs)
  (puthash "nor" "⊽" xmsi-abrvs)

(puthash "triangle" "▲" xmsi-abrvs)
(puthash "tri" "▲" xmsi-abrvs)
(puthash "tril" "◀" xmsi-abrvs)
(puthash "trir" "▶" xmsi-abrvs)
(puthash "trid" "▼" xmsi-abrvs)

(puthash "square" "■" xmsi-abrvs)
(puthash "circle" "●" xmsi-abrvs)
(puthash "diamond" "◆" xmsi-abrvs)
(puthash "star" "★" xmsi-abrvs)
(puthash "spade" "♠" xmsi-abrvs)
(puthash "club" "♣" xmsi-abrvs)
(puthash "heart" "♥" xmsi-abrvs)
(puthash "diam" "♦" xmsi-abrvs)

(puthash "<3" "♥" xmsi-abrvs)


;; full width characters
(puthash "fw," "，" xmsi-abrvs)
(puthash "fw." "．" xmsi-abrvs)
(puthash "fw:" "：" xmsi-abrvs)
(puthash "fw;" "；" xmsi-abrvs)
(puthash "fw!" "！" xmsi-abrvs)
(puthash "fw?" "？" xmsi-abrvs)
(puthash "fw`" "｀" xmsi-abrvs)
(puthash "fw'" "＇" xmsi-abrvs)
(puthash "fw\"" "＂" xmsi-abrvs)
(puthash "fw&" "＆" xmsi-abrvs)

(puthash "fw(" "（）" xmsi-abrvs)
(puthash "fw)" "）" xmsi-abrvs)
(puthash "fw[" "［］" xmsi-abrvs)
(puthash "fw]" "］" xmsi-abrvs)
(puthash "fw{" "｛｝" xmsi-abrvs)
(puthash "fw}" "｝" xmsi-abrvs)

(puthash "fw@" "＠" xmsi-abrvs)
(puthash "fw^" "＾" xmsi-abrvs) ; CIRCUMFLEX ACCENT
(puthash "fw`" "｀" xmsi-abrvs) ; GRAVE ACCENT
(puthash "fw~" "～" xmsi-abrvs)
(puthash "fw_" "＿" xmsi-abrvs)
(puthash "fw¯" "￣" xmsi-abrvs) ; MACRON

(puthash "fw#" "＃" xmsi-abrvs)
(puthash "fw+" "＋" xmsi-abrvs)
(puthash "fw-" "－" xmsi-abrvs)
(puthash "fw*" "＊" xmsi-abrvs)
(puthash "fw=" "＝" xmsi-abrvs)
(puthash "fw<" "＜" xmsi-abrvs)
(puthash "fw>" "＞" xmsi-abrvs)
(puthash "fw%" "％" xmsi-abrvs)

(puthash "fw|" "｜" xmsi-abrvs)
(puthash "fw¦" "￤" xmsi-abrvs)
(puthash "fw/" "／" xmsi-abrvs)
(puthash "fw\\" "＼" xmsi-abrvs)
(puthash "fw¬" "￢" xmsi-abrvs)

(puthash "fw((" "｟" xmsi-abrvs)
(puthash "fw))" "｠" xmsi-abrvs)

(puthash "fw$" "＄" xmsi-abrvs)
(puthash "fw£" "￡" xmsi-abrvs)
(puthash "fw¢" "￠" xmsi-abrvs)
(puthash "fw₩" "￦" xmsi-abrvs) ; WON SIGN
(puthash "fw¥" "￥" xmsi-abrvs) ; YEN SIGN

(puthash "fw0" "０" xmsi-abrvs)
(puthash "fw1" "１" xmsi-abrvs)
(puthash "fw2" "２" xmsi-abrvs)
(puthash "fw3" "３" xmsi-abrvs)
(puthash "fw4" "４" xmsi-abrvs)
(puthash "fw5" "５" xmsi-abrvs)
(puthash "fw6" "６" xmsi-abrvs)
(puthash "fw7" "７" xmsi-abrvs)
(puthash "fw8" "８" xmsi-abrvs)
(puthash "fw9" "９" xmsi-abrvs)

(puthash "fwA" "Ａ" xmsi-abrvs)
(puthash "fwB" "Ｂ" xmsi-abrvs)
(puthash "fwC" "Ｃ" xmsi-abrvs)
(puthash "fwD" "Ｄ" xmsi-abrvs)
(puthash "fwE" "Ｅ" xmsi-abrvs)
(puthash "fwF" "Ｆ" xmsi-abrvs)
(puthash "fwG" "Ｇ" xmsi-abrvs)
(puthash "fwH" "Ｈ" xmsi-abrvs)
(puthash "fwI" "Ｉ" xmsi-abrvs)
(puthash "fwJ" "Ｊ" xmsi-abrvs)
(puthash "fwK" "Ｋ" xmsi-abrvs)
(puthash "fwL" "Ｌ" xmsi-abrvs)
(puthash "fwM" "Ｍ" xmsi-abrvs)
(puthash "fwN" "Ｎ" xmsi-abrvs)
(puthash "fwO" "Ｏ" xmsi-abrvs)
(puthash "fwP" "Ｐ" xmsi-abrvs)
(puthash "fwQ" "Ｑ" xmsi-abrvs)
(puthash "fwR" "Ｒ" xmsi-abrvs)
(puthash "fwS" "Ｓ" xmsi-abrvs)
(puthash "fwT" "Ｔ" xmsi-abrvs)
(puthash "fwU" "Ｕ" xmsi-abrvs)
(puthash "fwV" "Ｖ" xmsi-abrvs)
(puthash "fwW" "Ｗ" xmsi-abrvs)
(puthash "fwX" "Ｘ" xmsi-abrvs)
(puthash "fwY" "Ｙ" xmsi-abrvs)
(puthash "fwZ" "Ｚ" xmsi-abrvs)
(puthash "fwa" "ａ" xmsi-abrvs)
(puthash "fwb" "ｂ" xmsi-abrvs)
(puthash "fwc" "ｃ" xmsi-abrvs)
(puthash "fwd" "ｄ" xmsi-abrvs)
(puthash "fwe" "ｅ" xmsi-abrvs)
(puthash "fwf" "ｆ" xmsi-abrvs)
(puthash "fwg" "ｇ" xmsi-abrvs)
(puthash "fwh" "ｈ" xmsi-abrvs)
(puthash "fwi" "ｉ" xmsi-abrvs)
(puthash "fwj" "ｊ" xmsi-abrvs)
(puthash "fwk" "ｋ" xmsi-abrvs)
(puthash "fwl" "ｌ" xmsi-abrvs)
(puthash "fwm" "ｍ" xmsi-abrvs)
(puthash "fwn" "ｎ" xmsi-abrvs)
(puthash "fwo" "ｏ" xmsi-abrvs)
(puthash "fwp" "ｐ" xmsi-abrvs)
(puthash "fwq" "ｑ" xmsi-abrvs)
(puthash "fwr" "ｒ" xmsi-abrvs)
(puthash "fws" "ｓ" xmsi-abrvs)
(puthash "fwt" "ｔ" xmsi-abrvs)
(puthash "fwu" "ｕ" xmsi-abrvs)
(puthash "fwv" "ｖ" xmsi-abrvs)
(puthash "fww" "ｗ" xmsi-abrvs)
(puthash "fwx" "ｘ" xmsi-abrvs)
(puthash "fwy" "ｙ" xmsi-abrvs)
(puthash "fwz" "ｚ" xmsi-abrvs)

(progn
(puthash "tv" "📺" xmsi-abrvs)

)

  ;; 2010-12-10. char to add
  ;; soft hyphen ­
  ;; ↥ ↧ ⇤ ⇥ ⤒ ⤓ ↨

  )

(defun xmsi-add-cycle (cycleList)
  "DOCSTRING"
  (let (
        (ll (- (length cycleList) 1) )
        (ξi 0)
        )
    (while (< ξi ll)
      (let (
            (charThis (elt cycleList ξi ))
            (charNext (elt cycleList (+ ξi 1) ))
            )
        (puthash charThis charNext xmsi-abrvs)
        (setq ξi (1+ ξi) ) ) )
    (puthash (elt cycleList ll) (elt cycleList 0) xmsi-abrvs)
    ))

;; cycle brackets
(xmsi-add-cycle ["〘〙" "〔〕"])
(xmsi-add-cycle ["«»" "《》"])
(xmsi-add-cycle ["‹›" "〈〉"])
(xmsi-add-cycle ["【】" "〖〗"])

;; cycle arrows
(xmsi-add-cycle ["←" "⇐"])
(xmsi-add-cycle ["↑" "⇑"])
(xmsi-add-cycle ["→" "⇒"])
(xmsi-add-cycle ["↓" "⇓"])
(xmsi-add-cycle ["↔" "⇔"])
(xmsi-add-cycle ["⇐" "←"])
(xmsi-add-cycle ["⇑" "↑"])
(xmsi-add-cycle ["⇒" "→"])
(xmsi-add-cycle ["⇓" "↓"])
(xmsi-add-cycle ["⇔" "↔"])

;; cycle black white chars
(xmsi-add-cycle ["■" "□"])
(xmsi-add-cycle ["●" "○"])
(xmsi-add-cycle ["◆" "◇"])
(xmsi-add-cycle ["▲" "△"])
(xmsi-add-cycle ["◀" "◁"])
(xmsi-add-cycle ["▶" "▷"])
(xmsi-add-cycle ["▼" "▽"])
(xmsi-add-cycle ["★" "☆"])
(xmsi-add-cycle ["♠" "♤"])
(xmsi-add-cycle ["♣" "♧"])
(xmsi-add-cycle ["♥" "♡"])
(xmsi-add-cycle ["♦" "♢"])

(xmsi-add-cycle ["✂" "✄"])              ;scissor
(xmsi-add-cycle ["↹" "⇥" "⇤"])          ; tab
(xmsi-add-cycle ["⏎" "↩" "↵" "⌤" "⎆"])     ; return/enter
(xmsi-add-cycle ["⌫" "⌦"])     ; delete
(xmsi-add-cycle ["↶" "⎌"])     ; undo
(xmsi-add-cycle ["✲" "⎈" "‸"])     ; control

(xmsi-add-cycle ["," "，"])
(xmsi-add-cycle ["·" "．" "。"])      ; MIDDLE DOT, FULLWIDTH FULL STOP, IDEOGRAPHIC FULL STOP
(xmsi-add-cycle [":" "："])    ; FULLWIDTH COLON
(xmsi-add-cycle [";" "；"])
(xmsi-add-cycle ["!" "！"])
(xmsi-add-cycle ["&" "＆" "﹠"])
(xmsi-add-cycle ["?" "？" "�"])
(xmsi-add-cycle [" " " " "　"])         ; space, NO-BREAK SPACE, IDEOGRAPHIC SPACE

(defun xmsi-hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (mylist)
    (maphash (lambda (kk vv) (setq mylist (cons (list vv kk) mylist))) hashtable)
    mylist
    )
  )

(defun xmsi-list-math-symbols ()
  "Print a list of math symbols and their input abbreviations.
See `xmsi-mode'."
  (interactive)

  (let (mylist mylistSorted)
    ;; get the hash table into a list
    (setq mylist (xmsi-hash-to-list xmsi-abrvs))

    ;; sort and print it out
    (setq mylistSorted (sort mylist (lambda (a b) (string< (car a) (car b)))) )

    (with-output-to-temp-buffer "*xmsi math symbol input*"

      (mapc (lambda (tt) "" (interactive)
              (princ (concat (car tt) " " (car (cdr tt)) "\n")) )
            mylistSorted) ) ) )

(defvar xmsi-keymap nil "Keymap for xmsi-math-symbols-input mode.")

(progn
  (setq xmsi-keymap (make-sparse-keymap))

  (define-key xmsi-keymap (kbd "S-SPC") 'xmsi-change-to-symbol)
  )

(defun xmsi-abbr-to-symbol (inputString)
  "Returns a char corresponding to inputString."
  (let (resultSymbol charByNameResult)
    (setq resultSymbol (gethash inputString xmsi-abrvs))
    (cond
     (resultSymbol resultSymbol)
     ;; decimal. 「945」 or 「#945」
     ((string-match "\\`#?\\([0-9]+\\)\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString))))
     ;; e.g. decimal with html entity markup. 「&#945;」
     ((string-match "\\`&#\\([0-9]+\\);\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString))))
     ;; hex number. e.g. 「x3b1」 or 「#x3b1」
     ((string-match "\\`#?x\\([0-9a-fA-F]+\\)\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString) 16)))
     ;; html entity hex number. e.g. 「&#x3b1;」
     ((string-match "\\`&#x\\([0-9a-fA-F]+\\);\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString) 16)))
     ;; unicode full name. e.g. 「GREEK SMALL LETTER ALPHA」
     ((and (string-match "\\`\\([- a-zA-Z0-9]+\\)\\'" inputString) (setq charByNameResult (assoc-string inputString (ucs-names) t) )) (char-to-string (cdr charByNameResult)))
     (t nil) )
     ) )

(defun xmsi-change-to-symbol (&optional print-message-when-no-match)
  "Change text selection or word to the left of cursor into a Unicode character.

A valid input can be any abbreviation listed by the command `xmsi-list-math-symbols', or, any of the following form:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If preceded by `universal-argument', print error message when no valid abbrev found.

See also: `xmsi-mode'."
  (interactive "P")
  (let (p1 p2 inputStr resultSymbol)
    (if (region-active-p)
        ;; if there's a text selection, then use that as input.
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (setq inputStr (buffer-substring-no-properties p1 p2) )
          (setq resultSymbol (xmsi-abbr-to-symbol inputStr))
          (when resultSymbol (progn (delete-region p1 p2) (insert resultSymbol)) ) )

        ;; if there's no text selection, grab all chars to the left of cursor point up to whitespace, try each string until there a valid abbrev found or none char left.
        (progn
          (setq p2 (point) )
          (skip-chars-backward "^ \t\n" -10)
          (setq p1 (point) )
          (while (and (not resultSymbol) (>= (- p2 p1) 1) )
            (setq inputStr (buffer-substring-no-properties p1 p2) )
            (setq resultSymbol (xmsi-abbr-to-symbol inputStr))
            (when resultSymbol (progn (goto-char p2) (delete-region p1 p2) (insert resultSymbol)) )
            (setq p1 (1+ p1)) ) ))

    (when (not resultSymbol)
      (when print-message-when-no-match (xmsi-list-math-symbols) (error "「%s」 is not a valid abbrevation or input. Call “xmsi-list-math-symbols” for a list. Or use a decimal e.g. 「945」 or hexadecimal e.g. 「x3b1」, or full Unicode name e.g. 「greek small letter alpha」."  inputStr))
      )
 ) )

(define-minor-mode xmsi-mode
  "Toggle math symbol input (minor) mode.

A mode for inputting a few math and other Unicode symbols.

Type “inf”, then press 【Shift+Space】, then it becomes “∞”.
Other examples:
 a ⇒ α
 p ⇒ π
 != ⇒ ≠
 >= ⇒ ≥
 => ⇒ ⇒
 -> ⇒ →
 and ⇒ ∧
etc.

If you have a text selection, then selected word will be taken as
input. For example, type 「sin(a)」, select the “a”, then press
 【Shift+Space】, then it becomse 「sin(α)」.

For the complete list of abbrevs, call `xmsi-list-math-symbols'.
All XML char entity abbrevs are supported. For example, 「copy」 ⇒ 「©」.

Decimal and hexadecimal can also be used. Example:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If you wish to enter a symbor by full unicode name but do not
know the full name, call `ucs-insert'. Asterisk “*” can be used
as a wildcard to find the char. For example, call
“ucs-insert”, then type 「*arrow」 then Tab, then emacs will list
all unicode char names that has “arrow” in it. (this feature is
part of Emacs 23)

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

Home page at: URL `http://ergoemacs.org/emacs/xmsi-math-symbols-input.html'"
  nil
  :global t
  :lighter " ∑"
  :keymap xmsi-keymap
  )

(provide 'xmsi-math-symbols-input)
