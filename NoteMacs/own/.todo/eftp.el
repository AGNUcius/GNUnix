;ftp://ftp.isi.edu/in-notes/rfc959.txt

(provide 'eftp)

(defvar eftp-process)

(defun eftp-open (host)
  (interactive "sFtp to Host: ")
  (setq eftp-process (open-network-stream "eftp" "eftp" host 21)))

(defun eftp-send (string)
  (interactive "s")
  (process-send-string eftp-process (concat string "\r\n")))

;(defun eftp-send (string)
;  (eftp-send-flap 2 (format "%s%c" string 0)))
;;;----------------------------------------------------------------------------
;;; FLAP Sender
;;;----------------------------------------------------------------------------


;             USER <SP> <username> <CRLF>
;             PASS <SP> <password> <CRLF>
;             ACCT <SP> <account-information> <CRLF>
;             CWD  <SP> <pathname> <CRLF>
;             CDUP <CRLF>
;             SMNT <SP> <pathname> <CRLF>
;             QUIT <CRLF>
;             REIN <CRLF>
;             PORT <SP> <host-port> <CRLF>
;             PASV <CRLF>
;             TYPE <SP> <type-code> <CRLF>
;             STRU <SP> <structure-code> <CRLF>
;             MODE <SP> <mode-code> <CRLF>
;             RETR <SP> <pathname> <CRLF>
;             STOR <SP> <pathname> <CRLF>
;             STOU <CRLF>
;             APPE <SP> <pathname> <CRLF>
;             ALLO <SP> <decimal-integer>
;                 [<SP> R <SP> <decimal-integer>] <CRLF>
;             REST <SP> <marker> <CRLF>
;             RNFR <SP> <pathname> <CRLF>
;             RNTO <SP> <pathname> <CRLF>
;             ABOR <CRLF>
;             DELE <SP> <pathname> <CRLF>
;             RMD  <SP> <pathname> <CRLF>
;             MKD  <SP> <pathname> <CRLF>
;             PWD  <CRLF>
;             LIST [<SP> <pathname>] <CRLF>
;             NLST [<SP> <pathname>] <CRLF>
;             SITE <SP> <string> <CRLF>
;             SYST <CRLF>
;             STAT [<SP> <pathname>] <CRLF>
;             HELP [<SP> <string>] <CRLF>
;             NOOP <CRLF>
