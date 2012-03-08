;;; benchmark.el --- functions for benchmarking Emacs Lisp code
;; Copyright (C) 1998 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: utilities

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To benchmark code, simply say
;;
;; (benchmark 4
;;  (sit-for 1))
;;
;; In this example, the `sit-for' function will be run four times.

;;; Code:

(defun benchmark-make-time-float (list)
  (+ (* 65536.0 (pop list)) (pop list)
     (/ (or (pop list) 0) 1e6)))

(put 'benchmark 'lisp-indent-function 1)
(put 'benchmark 'edebug-form-spec '(form body))

;;;###autoload
(defmacro benchmark (n &rest forms)
  "Execute FORMS N times and return the elapsed time.
FORMS will be byte-compiled before they are executed."
  `(benchmark-1 ,n ',forms))

(defun benchmark-1 (n forms)
  (let ((bi (make-symbol "benchmark-i")))
    (let ((func
	   (byte-compile
	    `(lambda ()
	       (let ((,bi ,n))
		 (while (not (zerop ,bi))
		   (setq ,bi (1- ,bi))
		   ,@forms)))))
	  t1 t2 result)
      (setq t1 (current-time))
      (funcall func)
      (setq t2 (current-time)
	    result (- (benchmark-make-time-float t2)
		      (benchmark-make-time-float t1)))
      (message "Time elapsed: %.4f" result)
      result)))

(provide 'benchmark)

;;; benchmark.el ends here
