;;; linki.el --- generate html using plain text documents and directory contents.  optional .linki rule file and simple markup included.

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Patrick Anderson <patware@freeshell.org>
;; Keywords: multimedia

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


;;; Customizations:
(defvar linki-output-dir "~/Homepage/")

;;; Code:

(defvar linki-mapping nil)

(defun linki ()
  (interactive)
  (setq linki-mapping nil)
  (linki-ls-rules ".")
  (linki-global-rules)
  (linki-.link-rules)
  (linki-make-pages))

;file-relative-name file dir)
(defun linki-recurse (process-file dir)
  (mapcar
   (lambda (token)
	 (if (not (or (string-equal token ".") (string-equal token "..")))
		 (progn
		   (if (char-equal (string-to-char dir) ?.)
			   (if (= 1 (length dir))
					  (setq dir (substring dir 1))
					  (setq dir (concat (substring dir 1) "/"))))
		   (let ((path (concat dir token)))
;		   (let ((path token)
;			   (url (concat (if (char-equal (string-to-char dir) ?.) "" (concat dir "/")) token)))
				 (funcall process-file token path)
				 (if (file-directory-p path)
					 (linki-recurse process-file (concat path "/")))))))
	 (directory-files dir)))

(defun linki-ls-rules (dir)
  (linki-recurse
   (lambda (token path)
	 (setq linki-mapping (cons (list (regexp-quote token) (concat "<a href=\"" path 
																  (if (file-directory-p token) "\">" ".htm\">")
																 token "</a>"))
	 linki-mapping)))
   "."
   ))

(defun linki-make-pages ()
  (linki-recurse
   (lambda (token path)
	 (let* ((outpath (concat linki-output-dir path))
			(htm (concat outpath ".htm")))
	   (if (file-directory-p path)
		   (if (not (file-exists-p outpath))
			   (make-directory outpath))
		 (copy-file path htm t)
		 (save-excursion
		   (set-buffer (find-file-noselect htm))
		   (mapcar
			(lambda (mapping)
			  (let ((token (car mapping))
					(link (car (cdr mapping))))
				(goto-char (point-min))
				(while (re-search-forward token nil t)
				  (replace-match link nil nil))))
			linki-mapping)
		   (save-buffer)
		   (kill-buffer (current-buffer))))))
   "."))

(defun linki-global-rules ()
  (setq linki-mapping
		(cons
		 '("\n" " <br>\n")
		 linki-mapping)))

(defun linki-.link-rules ()
  "Add .linki file contents to mapping."
)

(provide 'linki)
;;; linki.el ends here
