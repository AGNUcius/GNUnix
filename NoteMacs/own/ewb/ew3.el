;;; ew3.el --- Asynchronously render XHTML,CSS,JS locally or over HTTP on TCP/IP.

;; Copyright (C) 2002  Personal Sovereignty Foundation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Author: Patrick Anderson (concat (nreverse (string-to-list "gro.scimoCocE@tunwO")))

;;; Commentary:
;; This is a rewrite of ewb.el

;;; Customizations:

;;; Code:

(defvar ew3-connection nil)

(defvar ew3-buffer-name "*ew3*")

(defun ew3-get-sentinel (process event)
  (message (format "%s %s" process event))
  (save-excursion
    (goto-char (point-min))
	(insert "connected")))

(defun ew3-get-filter ()
  nil)

(defun ew3-open (host)
  "get over http"
  (interactive "sHost:")
  (message (concat "downloading " "..."))

  (if ew3-connection
	  (progn
		(delete-process ew3-connection)
		(setq ew3-connection nil)))

  (setq ew3-connection
		(open-network-stream-nowait
		 ew3-buffer-name 
		 (switch-to-buffer ew3-buffer-name) 
		 host
		 80
		 'ew3-get-sentinel
		 'ew3-get-filter)))

;; modified version of http://www.emacswiki.org/elisp/http-get.el

(defun http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9))
			   (= c ?-)
			   (= c ?_)
			   (= c ?.))
		       (string c)
		     (format "%%%02x" c)))
			 (encode-coding-string str content-type t))))

(defun ew3-get
  (process-send-string
   ew3-connection
   (concat
;;	"GET "
;;	(replace-regexp-in-string " " "%20" (concat dir file))
	"GET /"
	" HTTP/1.1\r\n"
	"Host: " host "\r\n"
	"User-Agent: Emacs Web Browser\r\n"
	"Accept: */*\r\n"
	"Accept-Language: *\r\n"
	"Accept-Encoding: gzip, deflate, compress\r\n"
	"Accept-Charset: ISO-8859-1, utf-8\r\n"
	"Keep-Alive: 300\r\n"
	"Connection: keep-alive\r\n"
	"\r\n")))

(provide 'ew3)
;;; ew3.el ends here
