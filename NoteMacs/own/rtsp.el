;;; rtsp.el --- rtsp client

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>
;; Keywords: network

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Version:
;; 1

;;; Commentary: 

;; links:
;; http://www.informit.com/articles/article.asp?p=169578&seqNum=3
;; http://www.cswl.com/whiteppr/tech/StreamingTechnology.html

;; from ~/comp/doc/rfc/rfc2326.txt

;;    The method token indicates the method to be performed on the resource
;;    identified by the Request-URI. The method is case-sensitive.  New
;;    methods may be defined in the future. Method names may not start with
;;    a $ character (decimal 24) and must be a token. Methods are
;;    summarized in Table 2.

;;       method            direction        object     requirement
;;       DESCRIBE          C->S             P,S        recommended
;;       ANNOUNCE          C->S, S->C       P,S        optional
;;       GET_PARAMETER     C->S, S->C       P,S        optional
;;       OPTIONS           C->S, S->C       P,S        required
;;                                                     (S->C: optional)
;;       PAUSE             C->S             P,S        recommended
;;       PLAY              C->S             P,S        required
;;       RECORD            C->S             P,S        optional
;;       REDIRECT          S->C             P,S        optional
;;       SETUP             C->S             S          required
;;       SET_PARAMETER     C->S, S->C       P,S        optional
;;       TEARDOWN          C->S             P,S        required



;;; Code:

(defvar rtsp-connection nil)

;; (defun rtsp-connect (host)
;;   (interactive "sURL: ")
;;   (let ((buf (get-buffer-create "*rtsp*")))
;; 	(setq rtsp-connection (open-network-stream "rtsp-connection" buf host 554))
;; 	(switch-to-buffer buf)
;; 	(erase-buffer)))

;; (defun rtsp-send (msg)
;;   (interactive "sMessage: ")
;;   (with-current-buffer (process-buffer rtsp-connection)
;; 	(erase-buffer)
;; 	(beginning-of-buffer)
;; 	(process-send-string rtsp-connection (concat msg "\r\n"))))



;;****************TESTING-BEG****************

(defun rtsp (host)
  (interactive "sURL: ")
  (let ((buf (get-buffer-create "*rtsp*")))
	(setq rtsp-connection (open-network-stream "rtsp-connection" buf host 554))
	(switch-to-buffer buf)
	(erase-buffer)
	(beginning-of-buffer)
	(process-send-string
	 rtsp-connection
	 "OPTIONS * RTSP/1.0\r\n\r\n")))
;;	 "OPTIONS * RTSP/1.0\r\nCSeq: 1\r\nRequire: implicit-play\r\n")))

real.sony.global.speedera.net
/real.sony.global/SystemOfADown/BOOMVidFull.smi



(defun rtsp (host)
  (interactive "sURL: ")
  (let ((buf (get-buffer-create "*rtsp*")))
	(setq rtsp-connection (open-network-stream "rtsp-connection" buf host 554))
	(switch-to-buffer buf)
	(erase-buffer)
	(beginning-of-buffer)
	(process-send-string
	 rtsp-connection
	 "DESCRIBE rtsp://streams2.omroep.nl/tv/vpro/tegenlicht/sb.20030516.rm\r\nCSeq: 1\r\n")))

(defun rtsp (host)
  (interactive "sURL: ")
  (let ((buf (get-buffer-create "*rtsp*")))
	(setq rtsp-connection (open-network-stream "rtsp-connection" buf host 554))
	(switch-to-buffer buf)
	(erase-buffer)
	(beginning-of-buffer)
	(process-send-string
	 rtsp-connection
	 "OPTIONS rtsp://realmedia.freespeech.org/ar_said_20010924.rm/ar_said_20010924.rm\r\nCSeq: 1\r\n")))

;; reply:
;; SET_PARAMETER * RTSP/1.0
;; CSeq: 1
;; Ping: Pong

;; SET_PARAMETER * RTSP/1.0
;; CSeq: 2
;; Ping: Pong

;;****************TESTING-END****************


(provide 'rtsp)
;;; rtsp.el ends here
