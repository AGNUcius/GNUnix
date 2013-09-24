;;; eco.el --- The Comical Ecology of Political Economy

;; Copyright (C) 2012

;; Author:  <AGNUcius@gmail.com>

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

;; A minor mode for Sharing Sources and Swapping Skills.

;;; Code:

(easy-mmode-define-minor-mode eco-mode
  "Toggle eco mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When eco mode is enabled, the control delete key
gobbles all preceding whitespace except the last.
See the command \\[eco-electric-delete]."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 " eco"
 ;; The minor mode bindings.
 '(("\C-\^?" . eco-electric-delete)
   ("\C-\M-\^?"
    . (lambda ()
        (interactive)
        (eco-electric-delete t)))))


(provide 'eco)
;;; eco.el ends here
