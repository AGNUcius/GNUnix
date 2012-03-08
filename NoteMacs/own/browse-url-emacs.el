;;; browse-url-emacs.el  --- use emacs's networking (no external program or lib) to dump web page to buffer
;;; inspired by Janardan Revuru's weather.el

;; Author: Patrick Anderson 
;; Home:   http://www.hobbiton.org/~adbuster/emacs
;; Version: 1

;install:
;this file in your load path

;add
; (autoload 'browse-url-emacs "browse-url-emacs" "use emacss to dump web page to buffer" t)
;to your .emacs file

;execute
; M-x eval-buffer
;so you don't have to restart

;problems:
;the address parsing is very bad - must be in the form  http://some.address.end/page.ext

;todo:
;fix/loosen parser
;use defcustom


(defvar http:// "http://")

;;;###autoload 
(defun browse-url-emacs(host-file)
  (interactive "sAddress:  \n")
  (if (compare-strings host-file () (length http://) http:// () () t)
	  (setq host-file (truncate-string host-file (length host-file) (length http://))))
  (let* (
		 (found/ (string-match "/" host-file))
		 (host (truncate-string host-file found/ ))
		 (file (truncate-string host-file (length host-file) (+ 1 found/))))
	(browse-url-emacs:GET host file)))
 
(defun browse-url-emacs:GET (host file)
  "GET file over http"
  (message (concat "downloading  " host "/" file "..."))
  (let ((tcp-connection) 
		(port 80)
		(buf (get-buffer-create (concat host "/" file))))
    (display-buffer buf)
    (or
	 (setq tcp-connection (open-network-stream "HTTP Process" buf host port))
;	  (set-marker (process-mark tcp-connection) (point-min)))
     (error "Could not open connection to %s:%d" host port))
	(process-send-string tcp-connection (concat "GET /" file " HTTP/1.0\n\n"))
	))
;todo:  try to font-lock the buffer according to file ext (assume index.html? if none)
;  	(font-lock-fontify-buffer))


(defun weather()
  "grab weather"
  (interactive)
  (browse-url-emacs "http://weather.unisys.com/foredat.cgi/KSLC"))


(provide 'browse-url-emacs)