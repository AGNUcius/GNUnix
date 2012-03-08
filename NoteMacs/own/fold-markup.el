;;; fold-markup.el --- minor mode to hide formatting, fill user tokens, provide traversal

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Patrick Anderson <>
;; Keywords: multimedia, languages, matching

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;;; Commentary:
;; this isn't yet working

;; thanks to jit-lock and lazy-lock, this is an interruptible parse engine

;;; Version
;; -1

;;; Code:

;tokens
;(fm-register name regexp) to create buffer-local vars which can be used by the fm-process-* fns

(defvar fm-position 1) ;make-local-variable
(make-variable-buffer-local 'fm-position)

(defvar eic-messages-exists nil)
(defvar fm-regexp "\\* \\([0-9]*\\) EXISTS")
(defvar fm-action 'font-lock-type-face)
(defvar fm-token 'eic-messages-exists)
(defvar fm-tokens '(
					("\\* \\([0-9]*\\) EXISTS" 'font-lock-type-face 'eic-messages-exists)
					("<[^>]*>\\|&[a-zA-Z0-9#]+;\\|\r" 'invisible nil)
					("<[aA][^>]*\\(?:[hH][rR][eE][fF]\\|[nN][aA][mM][eE]\\)=\"?\\([^\">]+\\)[^>]*>\\([^<]*\\)" "</[aA]>" (nil 'font-lock-keyword-face) 'fm-traverse-push)


(defvar fm-traverse-list nil)
(defvar fm-traverse-cur 1)

(setq fm-traverse-list '((5 "http://www.hi.com") (100 "bye")))
(setq fm-traverse-cur 0)

;traversal
(defun fm-traverse-next ()
  (let ((cur (nth fm-traverse-cur fm-traverse-list)))
	(goto-char (car cur))
	(message (car (cdr cur)))))

(defun fm-traverse-prev ())
(defun fm-traverse-go ())
(defun fm-traverse-back ())

(defun fm-traverse-push (link beg end)
  "push link onto traversable list"
  (setq fm-traverse-list (cons (list link loc) fm-traverse-list)))

(defcustom fm-sit-for 0.125
  "seconds to sit"
  :group 'fm-mode)

(defcustom fm-chunk-size 100
  "bytes to eat at once"
  :group 'fm-mode)

(defcustom fm-stealth-time 2
  "seconds to wait before beginning the parse."
  :group 'fm-mode)

(defvar fm-stealth-timer nil)

(defun fm-minor-mode
  (if (null fm-stealth-timer)
	  (progn
		(add-hook 'after-change-functions 'fm-after-change nil t)
		(setq fm-stealth-timer
			  (run-with-idle-timer fm-stealth-time
								   fm-stealth-time
								   'fm-parse-buffer)))
	(remove-hook 'after-change-functions 'fm-after-change t)
	(cancel-timer fm-stealth-timer)))

(defun fm-after-change (start end old-len)
  nil)

(defun fm-parse-buffer ()
  "begin parsing as `input-pending-p' and `load-average' dictate."
  (save-excursion
    (while (and (< (car (load-average)) fm-stealth-load)
				(not (input-pending-p))
				executing-kbd-macro)
	  (message "fm-parsing stealthily...")
		  (fm-parse-chunk)
		  (sit-for fm-sit-for))))

(defun fm-parse-chunk ()
  (interactive)
  (save-excursion
	(save-match-data
	  (goto-char fm-position)
	  (if (re-search-forward fm-regexp (+ fm-position fm-chunk-size) t)
		  (progn
			(if (facep fm-action)
				(overlay-put 
				 (make-overlay 
				  (match-beginning 1) (match-end 1)) 'face fm-action))
			(set fm-token (match-string 1)))))))


(provide 'fm-mode)
;;; fold-markup.el
