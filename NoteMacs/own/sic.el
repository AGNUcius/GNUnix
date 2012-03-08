;;; sic.el --- simple IMAP client (RFC 3501)

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <Ownut@EcoComics.org>
;; Keywords: mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Version
;; -.4 cleanup
;; -.5 very broken (heading toward version 0)
;; -.6 pre Alpha

;;; Use (Press 'C-l' after each command to refresh the screen.)

;; M-x sic-connect  RET  Mail.FreeShell.org  RET  143  RET

;; Press 's' to send a request to the server.
;; The first request should be: "login USERNAME PASSWORD"

;; Press 'g' to revert the buffer (show the status of the account)

;; Press 'l' to list the titles of the most recent messages.
;; Within the LIST screen:
;;   'RET' or mouse-2 retreives that message
;;   'TAB' and shift-TAB moves between messages
;;   'd' marks that message for Deletion
;;   'c' Clears the deletion mark of that message
;;   'x' eXpunges marked messages

;; 'C-h m' for keyboard help

;;; Code
(require 'interpreter-minor)

(defun sic-follow ()
  "Fetch message at point"
  (sic-send (concat "fetch " (current-word) " rfc822 (TEXT)")))

(im-make-mode sic "simple imap client" "\\(^\* \\)\\([0-9]+\\)" sic-follow)

(defvar sic-del-pending nil "list of messages to be flag \\deleted")

(define-key sic-mode-map [(d)]
  (lambda () (interactive)
;; 	(add-to-list 'sic-del-pending (current-word))))
	(sic-send (concat "store " (current-word) " +flags (\\deleted)"))))

(define-key sic-mode-map [(c)]
  (lambda () (interactive)
	(remove (current-word) sic-del-pending)))

(define-key sic-mode-map [(meta control backspace)]
  (lambda () (interactive)
	"unmark all"
	;;iterate through sic-del-pending sending -flags \\deleted to
	;;each, then set `sic-del-pending' to nil
	(setq sic-del-pending nil)))

	(define-key sic-mode-map [(c)]
	  (lambda () "clear `delete' flag" (interactive)
		(sic-send (concat "store " (current-word) " -flags (\\deleted)"))))

;; This is not yet safe
;; (define-key sic-mode-map [(x)]
;;   (lambda () "Permanent deletion" (interactive)
;; 	 (sic-send "expunge")))

(defun sic-revert ()
  (interactive)
  (sic-send "select inbox"))
(define-key sic-mode-map [(g)] 'sic-revert)

(defun sic-list ()
  (interactive)
  (sic-update-exists)
  ;; root:   "LIST \"\" %"
  ;; unseen: "search
  ;; FETCH 2:4 (FLAGS BODY[HEADER.FIELDS (DATE FROM)])
  ;;  (sic-send (concat "FETCH " (int-to-string (- sic-exists 5))  ":" (int-to-string sic-exists) " (FLAGS BODY[HEADER.FIELDS (DATE FROM SUBJECT)])"))
  ;;  (sic-send (concat "FETCH " (int-to-string (- sic-exists 5))  ":" (int-to-string sic-exists) " (FLAGS BODY[HEADER.FIELDS (DATE FROM SUBJECT)] BODY.PEEK[TEXT]<0.100>)"))
  ;;  (sic-send (concat "FETCH " (int-to-string (- sic-exists 5))  ":" (int-to-string sic-exists) " (FLAGS BODY[HEADER.FIELDS (DATE FROM SUBJECT)] BODY.PEEK[TEXT]<0.256>)"))
  ;;  (sic-send (concat "fetch " (int-to-string (- sic-exists 5))  ":" (int-to-string sic-exists) " (flags rfc822.header.lines (subject from))"))
  (sic-send (concat "FETCH " (int-to-string (- sic-exists 5))  ":" (int-to-string sic-exists) " BODY[HEADER.FIELDS (FROM SUBJECT)]")) ;;(FLAGS rfc822.header.lines (subject from))"))
  (goto-char (point-min)))
(define-key sic-mode-map [(l)] 'sic-list)

(defun sic-open (msg)
  (interactive "nMessage#: ")
  (sic-send (concat "fetch " (int-to-string msg) " rfc822")))
;;(sic-send (concat "fetch " (int-to-string msg) ":" (int-to-string (- msg 1)) " (flags rfc822.header.lines (subject from))"))
(define-key sic-mode-map [(o)] 'sic-open)
(define-key sic-mode-map [(s)] 'sic-send)

(defgroup sic nil "simple IMAP client" :group 'mail)
(defcustom sic-host "mail.freeshell.org" "imap server")
(defcustom sic-update-interval 240 "seconds between updates")

(defvar sic-exists nil)
(defvar sic-connection nil) ;todo: should be list of processes

(defun sic-update-exists ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
	(if (re-search-forward "\\* \\([0-9]*\\) EXISTS" nil t)
		(setq sic-exists (string-to-int (match-string 1))))))

(defun sic-connect (host port)
  (interactive "sHost: \nnPort (probably 143): ")
  (let ((buf (get-buffer-create (concat "*sic "host ":" (int-to-string port) "*"))))
	(setq sic-connection (open-network-stream "sic-connection" buf host port)) ;todo: how do I make the default value 143?
	(switch-to-buffer buf)
	(erase-buffer)
	(sic-mode)))

;;(run-at-time sic-update-interval sic-update-interval 'sic-revert)))

(defun sic-send (msg)
  (interactive "sMessage: ")
  (with-current-buffer (process-buffer sic-connection)
	(erase-buffer)
	(beginning-of-buffer)
	(process-send-string sic-connection (concat "a "msg "\r\n"))))


(provide 'sic)
;;; sic.el ends here
