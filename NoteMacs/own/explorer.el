;;; explorer.el --- make `dired' act more like explorer.exe

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>

;; Keywords: convenience, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; ChangeLog:
;; Version 1.0: initial release

;; Installation:
;; Add the following to your .emacs file
;; (require 'explorer)

;;; Commentary:
;; messes with `dired-mode-map' bindings:
;; Return, Double-Down-Mouse-1 = explorer-shell-execute.  Use C-m to edit file in Emacs.  Use v to view.
;; Backspace = dired-up-directory
;; Meta Return = props.exe
;; Apps, Down-Mouse-2, Down-Mouse-3  = context.exe

;; todo:
;;   fix explorer-start-process


;;; Code:
(add-hook 'dired-load-hook 
		  (lambda ()
			(define-key dired-mode-map [(backspace)] 'dired-up-directory)
			(define-key dired-mode-map [(meta return)] 
			  '(lambda ()
				 (interactive)
				 (explorer-call-process "PropsFor.exe")))

			;;(define-key dired-mode-map [(delete)] 'recycle)

			;;use Ctrl-m or v to stay in The One True Editor
			(define-key dired-mode-map [(return)] 'explorer-shell-execute)
			(define-key dired-mode-map [double-down-mouse-1] 'explorer-shell-execute)
			(define-key dired-mode-map [apps] 
			  '(lambda ()
				 (interactive)
				 (explorer-call-process "context.exe")))

			(define-key dired-mode-map [down-mouse-2] 'explorer-context-menu)
			(define-key dired-mode-map [down-mouse-3] 'explorer-context-menu)))

(defun explorer-shell-execute ()
  "Call w32-shell-execute on current file"
  (interactive)
  (dired-map-over-marks
   (w32-shell-execute
	nil
	(replace-regexp-in-string
	 "/" "\\" (dired-get-filename nil t)nil t)) nil))

(defun explorer-context-menu (event)
  (interactive "e")
  (mouse-set-point event) ;; move point to clicked row
  (explorer-call-process "context.exe"))

(defun explorer-call-process (proc)
  (interactive)
  (call-process-shell-command
   (concat proc " \""
		   (replace-regexp-in-string "/" "\\" (dired-get-filename nil t)nil t)
		   "\"")))


;;not working:
(defun explorer-start-process (proc)
  (interactive)
  (start-process proc (concat "*" proc "*") proc
				 (concat " \""
						 (replace-regexp-in-string "/" "\\" (dired-get-filename nil t)nil t)
						 "\"")))

(provide 'explorer)
