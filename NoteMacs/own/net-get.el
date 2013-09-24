;;; net-get.el --- Get files from the network.

;; Copyright (C) 2011  

;; Author:  <Administrator@OX980>
;; Keywords: 

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

;; 

;;; Code:

(defvar net-connection nil)
(defvar net-buf (get-buffer-create "*Emacs Net-Get*"))

(defun net-get-http (host dir file)
  "get over http"
  (message (concat "downloading " file "..."))
	(if net-connection
		(progn
		  (delete-process net-connection)
		  (setq net-connection nil)))

	(setq net-connection (open-network-stream "net-connection" net-buf host 80))
	(process-send-string
	 net-connection
	 (concat
	  "GET "
	  (replace-regexp-in-string " " "%20" (concat dir file))
	  " HTTP/1.1\r\n"
	  "Host: " host "\r\n"
	  "User-Agent: Emacs Net-Get Library\r\n"
	  "Accept: */*\r\n"
	  "Accept-Language: *\r\n"
	  "Accept-Encoding: gzip, deflate, compress\r\n"
	  "Accept-Charset: ISO-8859-1, utf-8\r\n"
	  "Keep-Alive: 300\r\n"
	  "Connection: keep-alive\r\n"
	  "\r\n")))



;; (net-get-http "EmacsWiki.org" "/emacs" "/MinEmacs")

(provide 'net-get)
;;; net-get.el ends here
