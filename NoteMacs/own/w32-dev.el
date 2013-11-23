;;; w32-dev.el --- Develop using Visual Studio style key bindings

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Links:

;;; Commentary:
;;; CAUTION, this file sets global keys.

;;; Version: 1.0

;;; Code:

(define-key global-map [(f4)] 'next-error)
(define-key global-map [(shift f4)] 'previous-error)

(global-set-key [(f5)] 'gud-cont)
(global-set-key [(f7)] 'compile)
(global-set-key [(f8)] 'gud-step)
;; (global-set-key [(f9)] 'gdb-toggle-breakpoint)
(global-set-key [(f9)] 'gud-break)
(global-set-key [(shift f9)] 'gud-tbreak)
(global-set-key [(f10)] (lambda () (interactive) (progn (gud-next 1) (gud-refresh))))
(global-set-key [(f11)] (lambda () (interactive) (progn (gud-step 1) (gud-refresh))))
(global-set-key [(shift f11)] (lambda () (interactive) (progn (gud-finish 1) (gud-refresh))))

(define-key global-map [(f12)]
  (lambda () (interactive) (find-tag (thing-at-point 'symbol))))
;; (define-key global-map [(f12)]
;;   (lambda () (interactive)
;; 	(find-tag (current-word) next-p regexp-p)))
(define-key global-map [(shift f12)]
  (lambda () (interactive) (tags-reset-tags-tables)))
(define-key global-map [(meta shift f12)]
  (lambda () (interactive) (shell-command "ctags -Re")))

;; (define-key global-map [(f12)] 'tag-dwim)

;; (define-key global-map [(meta f12)] 'pop-tag-mark) ;M-*
;; (define-key global-map [(control f12)] (lambda () "find next tag" (interactive) (find-tag (current-word) t)))
;; (define-key global-map [(shift f12)] (lambda () "find prev tag" (interactive) (find-tag (current-word) -1)))
;; (define-key global-map [(shift control f12)] 'select-tags-table)
;;(define-key global-map [(f4)] 'tags-loop-continue) ;M-,


;; ;; Bookmarks
;; (define-key global-map [(shift f2)]
;;   (lambda () (interactive) (bookmark-delete (bookmark-make-name))))
;; (define-key global-map [(control f2)]
;;   (lambda () (interactive) (bookmark-set (bookmark-make-name))))
;; (define-key global-map [(f2)] 'bookmark-get-next)
;; (define-key global-map [(control shift f2)] 'list-bookmarks)

;; (defun bookmark-make-name() ;todo:  this doesn't always work when deleting, because if the file has been edited, the name and location may be out of synch.  try using 'bookmark-current-bookmark
;;   "create bookmark name based on file/line"
;;   (interactive)
;;   (concat (bookmark-buffer-file-name) ": "(what-line)))

;; (defvar bookmark-name-next 0
;;   "this is the zero-based integer representing the 'next' bookmark")

;; (defun bookmark-get-next()
;;   "Get the next bookmark as we traverse them all.
;; Jump to the first when we reach the end."
;;   (interactive)
;;   (if (< bookmark-name-next (- (safe-length (bookmark-all-names)) 1))
;; 	  (setq bookmark-name-next (+ 1 bookmark-name-next))
;; 	(setq bookmark-name-next 0))
;;   (bookmark-jump
;;    (nth bookmark-name-next (bookmark-all-names))))

(provide 'w32-dev)
;;; w32-dev.el ends here
