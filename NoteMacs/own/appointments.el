;;; appointments.el --- addons to appt-*

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Patrick Anderson <panderson@none.com>
;; Keywords: convenience

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

(defun appt-list()
  "simply outputs the list of pending appts"
  (interactive)
  (with-output-to-temp-buffer
	  "*Pending Appointments*"
	(appt-list-print appt-time-msg-list)))

(defun appt-list-print (l)
  (if l									;if of some length
	  (progn
		(print (car (cdr (car l))))		; print the first entry first,
		(appt-list-print (cdr l)))))		;then look at the rest of the list


(provide 'appointments)
;;; appointments.el ends here
