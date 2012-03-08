;;; font-control.el --- control frame size and position

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;;; Author: Patrick Anderson
;;; Versions:
;; 1.0 new

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; I don't understand fonts, fontsets, character encodings, buffer encodings, input methods...

(defun font-family-string ()
  (interactive)
  (insert (prin1-to-string(w32-select-font))))


;; setting the font in the following way competes with how `defcustom' sets the `default' font
;;(add-hook 'desktop-delay-hook 'frame-maximize)

;; ;				   (set-default-font (first final-frame-params)) ;do font first - as it will goof with the frame size
;; ;				   (modify-frame-parameters (selected-frame)
;; ;											(list (cons 'font (first final-frame-params))))
;; 				   (set-face-font 'default (first final-frame-params))

;; 				   (set-frame-size (selected-frame) (fourth final-frame-params) (fifth final-frame-params))
;; 				   (set-frame-position (selected-frame) (max (eval (second final-frame-params)) 0)	(max (eval (third final-frame-params)) 0))

;; 				 (frame-parameter (selected-frame) 'left))))
;; 						  (frame-parameter (selected-frame) 'font)
;; 						  (frame-parameter (selected-frame) 'left) ;x
;; 						  (frame-parameter (selected-frame) 'top) ;y
;; 						  (frame-width)				;width
;; 						  (frame-height)				;height




;;from RobertPraetorius.emacs
;; (font-lock-make-faces) ; need to create the faces before fonts can be set

;; (set-face-font 'font-lock-function-name-face
;;                "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")
;; (set-face-font 'font-lock-comment-face
;;                "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;; (set-face-font 'font-lock-string-face
;;                "-*-Lucida Console-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;; (set-face-font 'font-lock-keyword-face
;;                "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;; (set-face-font 'font-lock-type-face
;;                "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;; (set-face-font 'font-lock-reference-face
;;                "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")




;; C-x C-f /some/nonexisting/file/name RET
;; C-u C-\ chinese-py RET
;; nihao                   (enter Chinese here)
;; C-x RET c utf-8 RET
;; C-x C-s

(defun my-fontset-menu ()
      (interactive)
      (x-popup-menu
      `((0 0) ,(selected-frame)) 
      (append x-fixed-font-alist
        (list (generate-fontset-menu)))))

;;;from w32-win.el:
;; (defun mouse-set-font (&rest fonts)
;;   "Select a font.
;; If `w32-use-w32-font-dialog' is non-nil (the default), use the Windows
;; font dialog to get the matching FONTS. Otherwise use a pop-up menu
;; \(like Emacs on other platforms) initialized with the fonts in
;; `w32-fixed-font-alist'."
;;   (interactive
;;    (if w32-use-w32-font-dialog
;;        (let ((chosen-font (w32-select-font (selected-frame)
;; 					   w32-list-proportional-fonts)))
;; 	 (and chosen-font (list chosen-font)))
;;      (x-popup-menu
;;       last-nonmenu-event
;;     ;; Append list of fontsets currently defined.
;;       ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
;;       (if (fboundp 'new-fontset)
;;       (append w32-fixed-font-alist (list (generate-fontset-menu)))))))
;;   (if fonts
;;       (let (font)
;; 	(while fonts
;; 	  (condition-case nil
;; 	      (progn
;;                 (setq font (car fonts))
;; 		(set-default-font font)
;;                 (setq fonts nil))
;; 	    (error (setq fonts (cdr fonts)))))
;; 	(if (null font)
;; 	    (error "Font not found")))))




;; (insert (prin1-to-string (x-list-fonts "*")))

;; this builds a list of available fonts (in a special format) from the list of dirs in 'bdf-directory-list
;; (describe-fontset)

;; (setq w32-bdf-filename-alist
;;       (w32-find-bdf-fonts bdf-directory-list))


;; (create-fontset-from-fontset-spec
;;  "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-fontset-most,
;;  latin-iso8859-2:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-2,
;;  latin-iso8859-3:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-3,
;;  latin-iso8859-4:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-4,
;;  cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-5,
;;  greek-iso8859-7:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-7,
;;  latin-iso8859-9:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-9,
;;  latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
;;  chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
;;  chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
;;  chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*
;;  ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode,
;;  japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
;;  japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*,
;;  katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
;;  lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1,
;;  thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1,
;;  tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1,
;;  tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0"
;;  t t)




;; (set-default-font "fontset-bdf")

;; (setq w32-use-w32-font-dialog nil)


;; (setq w32-fixed-font-alist
;;       (append w32-fixed-font-alist
;;               '(("intlfonts"
;; 				 ("lao" "-*-fixed-medium-r-normal-*-24-*-mulelao-1")
;; 				 ("tibetan" "-*-fixed-medium-r-normal-*-24-*-muletibetan-0")
;; 				 ("tibetan-1-column" "-*-medium-r-normal-*-24-*-muletibetan-1")
;; 				 ("thai-tis620" "-*-medium-r-normal-*-24-*-tis620.2529-*")
;; 				 ("korean-ksc5601" "-*-mincho-medium-r-normal-*-24-*-ksc5601*-*")
;; 				 ("japanese-jisx0212" "-fixed-medium-r-normal-*-24-*-jisx0212*-*")
;; 				 ("japanese-jisx0208" "-fixed-medium-r-normal-*-24-*-jisx0208*-*")
;; 				 ("latin-jisx0201" "-fixed-medium-r-normal-*-24-*-jisx0201*-*")
;; 				 ("katakana-jisx0201" "-fixed-medium-r-normal-*-24-*-jisx0201*-*")
;; 				 ("chinese-big5-1" "-*-fixed-medium-r-normal-*-24-*-big5.eten-0")
;; 				 ("chinese-big5-2" "-*-fixed-medium-r-normal-*-24-*-big5.eten-0")
;; 				 ("chinese-gb2312" "-*-medium-r-normal-*-24-*-gb2312*-*")
;; 				 ("chinese-cns11643-1" "-*-medium-r-normal-*-24-*-cns11643*-1")
;; 				 ("chinese-cns11643-2" "-*-medium-r-normal-*-24-*-cns11643*-2")
;; 				 ("chinese-cns11643-3" "-*-medium-r-normal-*-24-*-cns11643*-3")
;; 				 ("chinese-cns11643-4" "-*-medium-r-normal-*-24-*-cns11643*-4")
;; 				 ("chinese-cns11643-5" "-*-medium-r-normal-*-24-*-cns11643*-5")
;; 				 ("chinese-cns11643-6" "-*-medium-r-normal-*-24-*-cns11643*-6")
;; 				 ("chinese-cns11643-7" "-*-medium-r-normal-*-24-*-cns11643*-")
;; ))))













;;;(set-default-font  "-*-Courier New-normal-r-*-*-12-90-96-96-c-*-iso8859-1") ;9

;;;(set-face-font 'italic "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
;;;(set-face-font 'italic "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
;;;(set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-11-*-*-*-c-*-iso8859-1")

;;; (set-default-font  "-*-Courier New-normal-r-*-*-13-90-96-96-c-*-iso8859-1")
;;; (set-default-font "-*-Courier New-normal-r-*-*-14-*-*-*-c-*-*-ansi-")
;;; (set-default-font "-*-Courier-normal-r-*-*-16-106-*-*-c-90-*-ansi-")
;;; (set-default-font "-*-Fixedsys-normal-r-*-*-15-100-*-*-c-80-*-ansi-")
;;; (set-default-font "-*-Terminal-normal-r-*-*-12-80-*-*-c-80-*-oem-")
;;; (set-default-font "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")

