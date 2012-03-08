;;; edebug-control.el --- MSVC key bindings for Elisp debugging

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author:
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(add-hook 'edebug-setup-hook
		  (lambda ()
			(interactive)
			(define-key emacs-lisp-mode-map [(f10)] 'edebug-defun)
			(define-key emacs-lisp-mode-map [(control f10)] 'edebug-goto-here)

			(define-key edebug-mode-map [(f5)] 'edebug-go-mode)
			(define-key edebug-mode-map [(shift f5)] 'top-level)

			(define-key edebug-mode-map [(f9)] 'edebug-set-breakpoint)
			(define-key edebug-mode-map [(shift f9)] 'edebug-unset-breakpoint)
			(define-key edebug-mode-map [(f10)] 'edebug-next-mode)
			(define-key edebug-mode-map [(f11)] 'edebug-step-mode)
			(define-key edebug-mode-map [(shift f11)] 'edebug-step-out)))


(provide 'edebug-control)
;;; edebug-control.el ends here
