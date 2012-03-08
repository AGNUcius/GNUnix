;;; hands.el --- image preview

;; Copyright 2004 Free Software Foundation, Inc

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; as in thumbs

;;; Code:

;; (insert (concat "  " (file-name-as-directory dir) ":\n"
;; 				"  total 25k\n"
;; 				"  drwxrwxrwx    0 Dec  5 13:20 .\n"
;; 				"  drwxrwxrwx    0 Dec  4 21:07 ..\n"))


;; (if prompt-p
;; 	(insert (concat "  -rw-rw-rw- 7.0k Dec  5 13:20 " fname "\n")))

;; ;;else, a dir?
;; (if prompt-p
;; 	(insert (concat "  drwxrwxrwx    0 Dec  4 21:07 " fname "\n")))

(defun hands ()
  (interactive)
  (let ((buf (get-buffer-create "*hands*")))
	(dired-map-over-marks
;; 	 (let ((file (replace-regexp-in-string
;; 				  "/" "\\" (dired-get-filename nil t) nil t)))
	 (let ((file (dired-get-filename nil t)))
	   (with-current-buffer buf
		 (progn
		   (end-of-buffer)
		   (insert-image-file file)
		   (end-of-line)
		   (insert file)
		   (insert "\n"))))
	 nil)))

;;; hands.el ends here
