;;; ptrv-dsp.el --- DSP conf

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

(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

;; Synth-A-Modeler mode
(setq auto-mode-alist (cons '("\\.mdl$" . sam-mode) auto-mode-alist))
(autoload 'sam-mode "sam-mode" "Synth-A-Modeler editing mode." t)


(provide 'ptrv-dsp)
;;; ptrv-dsp.el ends here
