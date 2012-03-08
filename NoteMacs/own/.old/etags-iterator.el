##DEPRECATED.  see w32-dev.el

;;; bookmark-iterator.el --- iterate through tags - similar to DevStudio

;; Author: Patrick Anderson 
;; Version: 1
;; License: GPL

;; install
;; this file in your load path

;; add
;;  (require 'etags-iterator)
;; to your .emacs file

;; type
;;  M-x eval-buffer
;; so you don't have to restart

;; todo:
;; write todo list

(require 'etags)

;; (defun etags.exe (dir)
;;   "run ctags binary"
;; 	(interactive "DRun etags on directory: ")
;; 	(cd dir)
;; 	(shell-command "cmd /c ctags -h .h.H.hh.hpp.hxx.h++ -e -R *")
;; 	(visit-tags-table default-directory))

;; (defun etags.exe (dir)
;;   "run ctags binary"
;; 	(interactive "DRun etags on directory: ")
;; 	(cd dir)
;; 	(shell-command "ctags *")
;; 	(visit-tags-table default-directory))

(defcustom etags-default-tables
  ' nil
;;   ("d:\\Program Files\\Microsoft Visual Studio\\VC98\\CRT\\SRC"
;; 		"~/dev/src/simple")
	  "list of dirs to pull TAGS files from"
	  :group 'etags
	  :type  '(repeat string))

(defun etags-set-default-tables ()
  (setq tags-table-list etags-default-tables))

(defun find-tag-current-word (&optional next-p regexp-p)
  "find the tag for the word currently under the cursor.
if none is found, call etags"
  (interactive)
  (find-tag (current-word) next-p regexp-p))
	
(define-key global-map [(f12)] 'find-tag-current-word) ;M-.
(define-key global-map [(meta f12)] 'pop-tag-mark) ;M-*
(define-key global-map [(control f12)] (lambda () "find next tag" (interactive) (find-tag (current-word) t)))
(define-key global-map [(shift f12)] (lambda () "find prev tag" (interactive) (find-tag (current-word) -1)))
(define-key global-map [(shift control f12)] 'select-tags-table)
(define-key global-map [(meta control f12)] 'tags-reset-tags-tables)
;;(define-key global-map [(meta control shift f12)] 'etags.exe)
;(define-key global-map [(f4)] 'tags-loop-continue)

(provide 'etags-iterator)
