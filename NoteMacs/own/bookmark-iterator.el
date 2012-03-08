##DEPRECATED.  see w32-dev.el
;;; bookmark-iterator.el --- iterate through bookmarks - similar to DevStudio

;; Author: Patrick Anderson 
;; Version: 1
;; License: GPL

;install:
;this file in your load path

;add
; (require 'bookmark-iterator)
;to your .emacs file

;execute
; M-x eval-buffer
;so you don't have to restart

;problems:
;deletion often fails because the location is dependent upon the name (very dumb)

;todo:
;make not so braindead


;todo:  alternative implementation when not visiting file:  use point-to-register
(defun bookmark-make-name() ;todo:  this doesn't always work when deleting, because if the file has been edited, the name and location may be out of synch.  try using 'bookmark-current-bookmark
  "create bookmark name based on file/line"
  (interactive)
  (concat (bookmark-buffer-file-name) ": "(what-line)))

(defun bookmark-set-by-line()
  "set and name bookmark according to the current file/line"
  (interactive)
  (bookmark-set
   (bookmark-make-name)))

(defvar bookmark-name-next 0
  "this is the zero-based integer representing the 'next' bookmark")

(defun bookmark-get-next()
  "Get the next bookmark as we traverse them all.
Jump to the first when we reach the end."
  (interactive)
  (if (< bookmark-name-next (- (safe-length (bookmark-all-names)) 1))
	  (setq bookmark-name-next (+ 1 bookmark-name-next))
	(setq bookmark-name-next 0))
  (bookmark-jump
   (nth bookmark-name-next (bookmark-all-names))))

(defun bookmark-delete-by-line()
  "Delete bookmark set at this file/line"
  (interactive)
  (bookmark-delete
   (bookmark-make-name)))

(define-key global-map [(shift f2)] 'bookmark-delete-by-line)
(define-key global-map [(control f2)] 'bookmark-set-by-line)
(define-key global-map [(f2)] 'bookmark-get-next)
(define-key global-map [(control shift f2)] 'list-bookmarks)

(provide 'bookmark-iterator)
