;;; time-control.el --- control time

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author:  <a@none.com>
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

  (let ((str (current-time-string)))
    (concat (if (equal ?\  (aref str 8))
                (substring str 9 10)
              (substring str 8 10))
            "-" (substring str 4 7) "-" (substring str 20 24)))



(provide 'time-control)
;;; time-control.el ends here
