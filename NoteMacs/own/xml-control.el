;;; xml-control.el --- xml stuff

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Patrick Anderson (concat (nreverse (string-to-list ">gro.llehseerf@erawtap<")) nil)

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

;; 
;; "<a href=\"\\([^\"]+?\\)\".*?>"
;; "\\1"

;;; Code:

(defun xml-beautify ()
  "unwind and indent"
  (interactive)
  (beginning-of-buffer)
    (while
	  (re-search-forward "/.*?>" nil nil)
	(replace-match "\\&"))

;;   (while
;; 	  (re-search-forward "><" nil nil)
;; 	(replace-match ">\n<"))
  (mark-whole-buffer)
  (indent-region))

(provide 'xml-control)
;;; xml-control.el ends here
