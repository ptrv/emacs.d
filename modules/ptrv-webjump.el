;;; ptrv-webjump.el --- webjump-conf

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

(after 'webjump
  (setq webjump-sites
        (append '(("Urban Dictionary" .
                   [simple-query
                    "www.urbandictionary.com"
                    "http://www.urbandictionary.com/define.php?term="
                    ""])
                  ("stackoverflow" .
                   [simple-query
                    "www.stackoverflow.com"
                    "http://stackoverflow.com/search?q="
                    ""])
                  ("askubuntu" .
                   [simple-query
                    "www.askubuntu.com"
                    "http://askubuntu.com/search?q="
                    ""])
                  ("superuser" .
                   [simple-query
                    "www.superuser.com"
                    "http://superuser.com/search?q="
                    ""])
                  ("tex.stackexchange" .
                   [simple-query
                    "tex.stackexchange.com"
                    "http://tex.stackexchange.com/search?q="
                    ""])
                  ("math.stackexchange" .
                   [simple-query
                    "math.stackexchange.com"
                    "http://math.stackexchange.com/search?q="
                    ""])
                  ("leo" .
                   [simple-query
                    "dict.leo.org"
                    "http://dict.leo.org/ende?search="
                    ""])
                  ("Java API" .
                   [simple-query
                    "www.google.com"
                    "http://www.google.ca/search?hl=en&as_sitesearch=http://java.sun.com/javase/6/docs/api/&q="
                    ""])
                  ("ClojureDocs" .
                   [simple-query
                    "clojuredocs.org"
                    "http://clojuredocs.org/search?q="
                    ""])
                  ("Clojars" .
                   [simple-query
                    "clojars.org"
                    "https://clojars.org/search?q="
                    ""]))
                webjump-sample-sites)))

(provide 'ptrv-webjump)
;;; ptrv-webjump.el ends here
