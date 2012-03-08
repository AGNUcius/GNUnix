;;; current-word-2-kill-ring.el --- 

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Patrick Anderson <panderson@none.com>
;; Keywords: 

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

;;; Code:

(defun current-word-2-kill-ring ()
  (interactive)
  (kill-append (current-word) nil))

;(define-key global-map 


(provide 'current-word-2-kill-ring)
;;; current-word-2-kill-ring.el ends here
