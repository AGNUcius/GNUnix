;;; sic.el --- simple imap client

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>
;; Keywords: comm, mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


(require 'ewb)

(defvar sic-mode-map (make-sparse-keymap))
(defvar imap-exists 1395)

(define-key ctl-x-map [(M)] (lambda () (interactive) (switch-to-buffer (process-buffer sic-connection))))
(define-key sic-mode-map [(c)] 'sic-connect)
(define-key sic-mode-map [(r)] (lambda () (interactive)(find-file "~/dev/doc/rfc/rfc1730.txt.bz2")))
(define-key sic-mode-map [(e)] (lambda () (interactive)(find-file "~/emacs/own-lisp/sic.el")))
(define-key sic-mode-map [(g)] (lambda () (interactive) (sic-send "z select inbox")))
(define-key sic-mode-map [(l)] 
  (lambda ()
	 (interactive)
	 (sic-send (concat "z fetch " (int-to-string (- imap-exists 6))  ":" (int-to-string imap-exists) " (flags rfc822.header.lines (subject from))"))))

(define-key sic-mode-map [(q)] 'bury-buffer)
(define-key sic-mode-map [(s)] 'sic-send)
(define-key sic-mode-map [(f)] 
  (lambda () 
	 (interactive)
	 (goto-char (point-min))
	 (while (re-search-forward "=.*\n" nil t)
	   (replace-match " " nil nil))
	 (goto-char (point-min))))

;(define-key sic-mode-map [(w)] (lambda () (interactive) (ewb-filter sic-connection "") (goto-char (point-min))))

(defgroup sic nil "simple IMAP client"
  :group 'mail)

(defcustom sic-host "iceman.none.com" "imap server")

(defun sic-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map sic-mode-map)
  (setq mode-name "sic")
  (setq major-mode 'sic-mode))

(defvar sic-connection nil) ;todo: should be list of processes

(defun sic-sentinel (process event)
  "remember, this is called _after_ sic-connection exits"
  (save-excursion
    (goto-char (point-min))
	(re-search-forward "\\* \\([0-9]*\\) EXISTS" nil t)
	(message "hi")
	(message (match-string 1))
	(setq imap-exists (string-to-int (match-string 1)))))

(defun sic-connect (host port)
  (interactive "sHost: \nsPort: ")
  (message (concat "connecting to " host ":" port))
  (let* (
		 (buf (get-buffer-create (concat "*sic "host ":" port "*"))))
	(setq sic-connection (open-network-stream "sic-connection" buf host 143)) ;todo broken interactive number
	(switch-to-buffer buf)
	(erase-buffer)
	(sic-mode)
;	(set-process-filter sic-connection 'ewb-filter)
;	(set-process-sentinel sic-connection 'sic-sentinel)
))

(defun sic-send (msg)
  (interactive "sMessage: ")
  (switch-to-buffer (process-buffer sic-connection))
  (erase-buffer)
  (process-send-string sic-connection (concat msg "\r\n")))

(provide 'sic)
;;; sic.el ends here
