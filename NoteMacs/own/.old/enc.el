;;; enc.el --- emacs network client

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>
;; Keywords: comm, network

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

(require 'ewb)

(defvar enc-mode-map (make-sparse-keymap))
(defvar imap-exists 1395)

(define-key ctl-x-map [(M)] (lambda () (interactive) (switch-to-buffer (process-buffer enc-process))))
(define-key enc-mode-map [(c)] 'enc-connect)
(define-key enc-mode-map [(r)] (lambda () (interactive)(find-file "~/dev/doc/rfc/rfc1730.txt.bz2")))
(define-key enc-mode-map [(e)] (lambda () (interactive)(find-file "~/emacs/own-lisp/enc.el")))
(define-key enc-mode-map [(g)] (lambda () (interactive)(enc-send "z select inbox")))
(define-key enc-mode-map [(l)] 
  (lambda ()
	 (interactive)
	 (enc-send (concat "z fetch " (int-to-string (- imap-exists 6))  ":" (int-to-string imap-exists) " (flags rfc822.header.lines (subject from))"))))

(define-key enc-mode-map [(q)] 'bury-buffer)
(define-key enc-mode-map [(s)] 'enc-send)
(define-key enc-mode-map [(f)] 
  (lambda () 
	 (interactive)
	 (goto-char (point-min))
	 (while (re-search-forward "=.*\n" nil t)
	   (replace-match " " nil nil))
	 (goto-char (point-min))))

(define-key enc-mode-map [(w)] (lambda () (interactive) (ewb-filter enc-process "") (goto-char (point-min))))

(defun enc-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map enc-mode-map)
  (setq mode-name "enc")
  (setq major-mode 'enc-mode))

(defvar enc-process nil) ;todo: should be list of processes

(defun imap-sentinel (process event)
  (princ (format "%s %s" process event))
  (save-excursion
    (goto-char (point-min))
	(re-search-forward "\* \([0-9]*\) exists" nil t)
	(setq imap-exists (match-string 1))))

(defun enc-connect (host port)
  (interactive "sHost: \nsPort: ")
  (message (concat "connecting to " host ":" port))
  (let* (
		 (buf (get-buffer-create (concat "*enc "host ":" port "*"))))
	(setq enc-process (open-network-stream "enc-process" buf host 143)) ;todo broken interactive number
	(switch-to-buffer buf)
	(erase-buffer)
	(enc-mode)
;	(set-process-filter enc-process 'ewb-filter)
	(set-process-sentinel enc-process 'imap-sentinel)
))

(defun enc-send (msg)
  (interactive "sMessage: ")
  (switch-to-buffer (process-buffer enc-process))
  (erase-buffer)
  (process-send-string enc-process (concat msg "\r\n")))

(provide 'enc)
;;; enc.el ends here
