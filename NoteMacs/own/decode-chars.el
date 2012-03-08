;;; decode-chars.el --- display and play with characters

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author:  <Administrator@none.com>
;; Keywords: faces, i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; discovered `decode-char' from http://www.emacswiki.org/elisp/typopunct.el

;;; Code:

;; (insert (decode-char 'ucs #x2026)) ;...
;; (insert (decode-char 'ucs #x10300)) ;;  '(("OLD ITALIC LETTER A" #x10300)

(defmacro decode-chars-paste (code-point)
  "paste "
  (insert (decode-char 'ucs ,code-point)))


(provide 'decode-chars)
;;; decode-chars.el ends here
