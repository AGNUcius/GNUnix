;;; interpreter-minor.el --- provide navigation, clickability, folding, interpretation

;; Copyright (C) 2002  Patware Unc.

;; Author: Patrick Anderson <patware@freeshell.org>
;; Keywords: convenience, multimedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; This isn't a minor mode at all, though that was my initial vision.
;; Right now it's more like easy-mmode.

;;; Todo:
;;   folding, interpretation using im-interpret-buffer

;;; Current:
;; 1.1 mouse-2 follows a link.

;;; Changes:
;; 1.0 The font-lock regexp can now be designated as a variable.

;;-.50 TAB jumps to the next link in the current document.
;;     RETURN follows a link.

;;; Code:
(defmacro im-make-mode (mode doc regexp follow)
  `(progn
	 (defvar ,(im-mm mode) (make-sparse-keymap))
	 (defun ,(im-make-sym mode "-mode") ()
	   ,(concat doc " \\{" (symbol-name mode) "-mode-map}")
	   (interactive)
	   (kill-all-local-variables)
	   (use-local-map ,(im-mm mode))
	   (setq mode-name ,(symbol-name mode))
	   (setq major-mode ',(im-make-sym mode "-mode"))
	   (add-to-invisibility-spec ',mode)
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
;;; Use `list-faces-display' to browse the available faces
;; 			 '('((,(eval regexp) (0 eshell-ls-executable-face font-lock-function-name-face))) t t nil))
;;			 '('((,(eval regexp) (0 font-lock-keyword-face))) t t nil))
;;			 '('((,(eval regexp) (0 font-lock-string-face))) t t nil))
 			 '('((,(eval regexp) (0 font-lock-comment-face))) t t nil))
;; 			 '('((,(eval regexp) (0 font-lock-function-name-face))) t t nil))
;;			 '('((,(eval regexp) (0 font-lock-preprocessor-face))) t t nil))
;; 			 '('((,(eval regexp) (0 mode-line-highlight))) t t nil))
	   ,(let ((mm (im-mm mode)))
		 `(progn
			(define-key ,mm [(9)]
			  (lambda ()
				 (interactive)
				 (goto-char (+ 1 (point)))
				 (re-search-forward ,regexp nil t)
				 (goto-char (match-beginning 2))))

			(define-key ,mm [(shift 9)]
			  (lambda ()
			  (interactive)
			  (re-search-backward ,regexp)
				 (goto-char (match-beginning 2))))

			(define-key ,mm [(return)]
			  (lambda ()
			  (interactive)
			  (,follow)))

			(define-key ,mm [(mouse-2)]
			  (lambda (event &optional raw)
			  (interactive "e\nP")
			  (goto-char (posn-point (event-end event)))
			  (,follow)))
			)))))

(defmacro im-make-key (mode key fn)
  `(define-key ,(im-mm mode) ,key
	 'lambda ()
	 (interactive)
	 ,fn))

(defun im-make-sym (mode ending)
  (intern (concat (symbol-name mode) ending)))

(defun im-mm (mode) (im-make-sym mode "-mode-map"))

(provide 'interpreter-minor)
;;; interpreter-minor.el ends here
