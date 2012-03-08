;;; frame-control.el --- control frame size, position and default face font-family

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Patrick Anderson

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;;; Commentary:

;; The problem is that I don't know how to specify the default face font-family through the `customize' interface.  The Attribute is listed there, but I just don't get it.

;; Set the default face colors by saying:
;;     M-x customize-face RET default RET

;;     Uncheck all but Foreground and Background Attributes
;;     Set Foreground and Background values by typing the color name.
;;     Choose a color name by saying:
;;         M-x list-colors-display RET

;;     Now make sure the "Font Family" attribute box is unchecked.
;;     Press "Save for Future Sessions" to write these changes to disk


;; Setting the default face font-family is clumsy.  We need the
;; font-name in a format consumable by `set-frame-font'.  To retreive
;; this string say:
;;     M-x customize-variable RET frame-font-string RET
;;     Move the cursor to the editable area and type:
;;         M-x font-family-string RET
;;         Choose the font you like, press 'OK'.
;;     Press "Save for Future Sessions"



;; Version:

;; 1.6: Restarted using defcustom instead of `desktop-globals-to-save'
;;      Font stuff is a little broken

;; 1.5: Retired.

;; 1.4: clean up installation.
;; 1.3: simplified install (now just copy one line to .emacs, and eval the next)
;; 1.3: added checks for running in terminal
;; 1.3: added checks for running on non-w32
;; 1.2: added font save/restore
;; 1.1: added more descriptive, correct installation docs


;;; Customizations:

(defgroup frame-control nil
  "Control Emacs frames and default face font-family"
  :group 'frames)

;;(defcustom frame-font-string "-outline-NSimSun-normal-r-normal-normal-29-217-96-96-c-*-iso8859-1"
(defcustom frame-font-string nil
  "This is any string accepted by `set-frame-font'"
  :type 'string
  :group 'frame-control)

(defcustom frame-control-startup? t
  "Used saved settings at startup?"
  :type 'bool
  :group 'frame-control)

(defcustom frame-control-bind-keys? t
  "Bind M-SPC to frame controls?"
  :type 'string
  :group 'frame-control)


;;; Prerequisites:
;;the following 2 lines must eval before font settings:
(setq w32-enable-italics t)
(setq w32-enable-synthesized-fonts t)

;;(require 'font-control)
(defun font-family-string ()
  (interactive)
  (insert (prin1-to-string(w32-select-font))))

;;; Code:
(if frame-control-bind-keys?
	(progn
	  (defvar frame-control-map (make-sparse-keymap))
	  (define-key global-map [(meta \ )] frame-control-map)
	  (define-key frame-control-map [(c)] 'save-buffers-kill-emacs)
	  (define-key frame-control-map [(n)] 'frame-minimize)
	  (define-key frame-control-map [(r)] 'frame-restore)
	  (define-key frame-control-map [(s)] 'frame-saver)
	  (define-key frame-control-map [(x)] 'frame-maximize)
	  (define-key frame-control-map [(return)] 'frame-restore)

;; 	  (define-key frame-control-map [(\ )] 'frame-menu)
;; 	  (define-key frame-control-map [(m)] 'frame-menu)

	  ))

(defun frame-control-startup ()
  (interactive)
  (add-hook 'after-init-hook 'frame-restore) ;;make sure we're not already maximized
  (run-with-idle-timer 
   .1 nil
   (lambda ()
	 (if frame-font-string
		 (set-default-font frame-font-string))
 	 (frame-maximize))))


(if frame-control-startup?
	(frame-control-startup))


;; '((menu ?\xf100)
;;   (saver ?\xf140)
;;   (minimize ?\xf020)
;;   (maximize ?\xf030)
;;   (restore ?\xf120))

(defun frame-maximize ()
  (interactive)
  (w32-send-sys-command ?\xf030))

(defun frame-restore ()
  (interactive)
  (w32-send-sys-command ?\xf120))

(defun frame-saver ()
  (interactive)
  (w32-send-sys-command ?\xf140))



;;NOTES:

;;#define SC_SIZE 0xF000
;;#define SC_MOVE 0xF010
;;#define SC_MINIMIZE 0xF020
;;#define SC_MAXIMIZE 0xF030
;;#define SC_NEXTWINDOW 0xF040
;;#define SC_PREVWINDOW 0xF050
;;#define SC_CLOSE 0xF060 (61536)
;;#define SC_VSCROLL 0xF070
;;#define SC_HSCROLL 0xF080
;;#define SC_MOUSEMENU 0xF090
;;#define SC_KEYMENU 0xF100
;;#define SC_ARRANGE 0xF110
;;#define SC_RESTORE 0xF120
;;#define SC_TASKLIST 0xF130
;;#define SC_SCREENSAVE 0xF140
;;#define SC_HOTKEY 0xF150
;;#define SC_DEFAULT 0xF160
;;#define SC_MONITORPOWER 0xF170
;;#define SC_CONTEXTHELP 0xF180
;;#define SC_SEPARATOR 0xF00F



;; this must be global - as that is how desktop-globals-to-save works
;; (defvar final-frame-params '((frame-parameter (selected-frame) 'font) 50 50 150 50 nil)) ;font, left, top, width, height, maximized

;; (defvar final-frame-params '("-adobe-courier-medium-r-normal--*-120-*-*-m-*-iso8859-1" 50 50 150 50 nil)) ;font, left, top, width, height, maximized

;; (if window-system
;; 	(add-hook 'desktop-delay-hook
;; 			  (lambda()
;; 				 "this is executed as emacs is coming up - _after_ final-frame-params have been read from `.emacs.desktop'."
;; 				 (when desktop-enable
;; 				   (desktop-load-default)
;; 				   (desktop-read)
;; ;;now size and position frame according to the  values read from disk
;; ;; 				   (set-default-font (first final-frame-params)) ;do font first - as it will goof with the frame size
;; ;; 				   (modify-frame-parameters (selected-frame)
;; ;; 											(list (cons 'font (first final-frame-params))))

;; 				   (set-face-font 'default (first final-frame-params))

;; 				   (set-frame-size (selected-frame) (fourth final-frame-params) (fifth final-frame-params))
;; 				   (set-frame-position (selected-frame) (max (eval (second final-frame-params)) 0)	(max (eval (third final-frame-params)) 0))

;; ;; 				   (if (sixth final-frame-params)
;; ;;  					   (if (eq window-system 'w32)
;; ;;  						   (w32-send-sys-command ?\xf030)
;; ;;  						 ) ;else, do X something
;; ;; 					 )
;; ))))

;; (if window-system
;; 	(add-hook 'desktop-save-hook
;; 			  (lambda()
;; 				 (let ((maximized (listp (frame-parameter (selected-frame) 'left))))
;; 				   "This runs during shutdown to set frame
;; 				   size/pos vars before `desktop.el' writes them
;; 				   to disk"
;; ;;  				   (if (eq window-system 'w32)
;; ;; 					   (w32-send-sys-command ?\xf120)	;restore the frame (so we can save the 'restored' size/pos)
;; ;; 					 ) ;else, do some X thing

;; ;;prepend our vars to the save list so `desktop.el' will save them out to disk
;; 				   (setq desktop-globals-to-save (cons 'final-frame-params
;; 													   desktop-globals-to-save))

;; 				   (setq final-frame-params
;; 						 (list
;; 						  (frame-parameter (selected-frame) 'font)
;; 						  (frame-parameter (selected-frame) 'left) ;;x
;; 						  (frame-parameter (selected-frame) 'top)  ;;y
;; 						  (frame-width)	  ;;width
;; 						  (frame-height)  ;;height
;; 						  maximized))))) ;;if this frame param is a list, we're probably maximized (not guaranteed)
;;   )

(provide 'frame-control)

;;; frame-control.el ends here
