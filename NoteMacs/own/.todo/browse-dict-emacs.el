
(defun dictionary(szWord)
  "retreive definition of szWord"
  (interactive "sWord:  \n")
  (let ((host "dict.org")
		(tcp-connection)
		(port 2628)
		(buf (get-buffer-create (concat "Definition of " szWord))))
	(message (concat "finding  " szWord " on " host "..."))

    (display-buffer buf)
    (or
	 (setq tcp-connection (open-network-stream "Dictionary Process" buf host port))
     (error "Could not open connection to %s:%d" host port))
	(process-send-string tcp-connection (concat "DEFINE ! " szWord))))
