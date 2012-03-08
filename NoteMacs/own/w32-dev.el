;;; w32-dev.el --- develop w32 apps using DevStudio 5, 6 or 7, SourceSafe, cdb Debugger


;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Links:
;; http://vcpp-mode.sourceforge.net


;;; Commentary:
;;; CAUTION this file sets global keys.

;;; Code:
;;; Compilation:
(define-key global-map [(f7)] 'compile)
(define-key global-map [(f4)] 'next-error)
(define-key global-map [(shift f4)] 'previous-error)

;;msdev6--------------------------------------
;(add-to-list 'exec-path "c:/Program Files/Microsoft Visual Studio/Common/MSDev98/Bin")
;;(setq compile-command "msdev.exe test.dsp /make \"test - win32 debug\" /rebuild")
;;msdev7--------------------------------------
;(add-to-list 'exec-path "c:/Program Files/Microsoft Visual Studio .NET 2003/Common7/IDE")
(setenv "PATH" (concat (getenv "PATH") ";d:\\Program Files\\Microsoft Visual Studio .NET 2003\\Common7\\IDE"))

;;devenv.exe outputs not to stdout, so use devenv.com
(setq compile-command "devenv.com test.vcproj /build debug")



;; Debugging:
;;cdb-gud.el:  Author: Stephan Doll <stephan_doll at dantz.com>
;;start from the project directory (don't cd to the debug dir first)
;; (setq gud-cdb-history '("~/comp/devl/debug/WinDBG/cdb.exe -v -G -o -lines debug\\test.exe"))

;; ;;once inside, use: > g main 
;; (load "cdb-gud")
;; (global-set-key [(f5)] 'gud-cont)
;; (global-set-key [(f8)] 'gud-step)
;; (global-set-key [(f9)] 'gud-break)
;; (global-set-key [(shift f9)] 'gud-tbreak)
;; (global-set-key [(f10)] (lambda () (interactive) (progn (gud-next 1) (gud-refresh))))
;; (global-set-key [(shift f10)] 'cdb)
;; (global-set-key [(f11)] (lambda () (interactive) (progn (gud-step 1) (gud-refresh))))
;; (global-set-key [(shift f11)] (lambda () (interactive) (progn (gud-finish 1) (gud-refresh))))


;;; Tags
(defun tags-build-tags-table (dir)
  "run ctags binary"
	(interactive "DRun etags on directory: ")
	(cd dir)
;;	(shell-command "cmd /c ctags -h .h.H.hh.hpp.hxx.h++ -e -R *")
;;	(shell-command "cmd /c ctags -e -R *")
;;	(shell-command "ctags.exe -e -R *")
;;	(call-process-shell-command "cmd /c ctags.exe -e -R *")
;;	(call-process-shell-command (concat "\"" (getenv "ProgramFiles")"\\Emacs23\\bin\\etags.exe *\""))
	(call-process-shell-command "ctags -e -R")
	(visit-tags-table default-directory))

(defun tag-find-current-word (&optional next-p regexp-p)
  "find the tag for the word currently under the cursor.
if none is found, call etags"
  (interactive)
  (find-tag (current-word) next-p regexp-p))

(define-key global-map [(meta control f12)] 'tags-build-tags-table)

;; (defun tag-dwim ()
;;   (if exists 
(define-key global-map [(f12)] 'tag-find-current-word) ;M-.
(define-key global-map [(meta control shift f12)] 'tags-reset-tags-tables)

(define-key global-map [(f12)] 'tag-dwim)

(define-key global-map [(meta f12)] 'pop-tag-mark) ;M-*
(define-key global-map [(control f12)] (lambda () "find next tag" (interactive) (find-tag (current-word) t)))
(define-key global-map [(shift f12)] (lambda () "find prev tag" (interactive) (find-tag (current-word) -1)))
(define-key global-map [(shift control f12)] 'select-tags-table)
;;(define-key global-map [(f4)] 'tags-loop-continue) ;M-,



;;; SourceSafe Version Control

;; vc-vss.el is fun, but makes file access (for me anyway) __slow__
;; (require 'vc-vss)

;; (setq
;; ;; not sure what to set vc-vss-path to...
;;  vc-vss-path "//YourSourceSafeServer.somewhere.com/someSSDatabaseShare/users/you/ss.ini" ;; This is an SMB path.  Does smbfs fix this for *nixes?
;;  vc-vss-path "c:/Program Files/Microsoft Visual Studio/VSS/srcsafe.ini" ;;could we use %ProgramFiles% (or is it $ProgramFiles)
;;  vc-vss-user "username,password" ;; careful where you store this!
;;  vc-default-back-end 'VSS
;;  diff-switches "-DV" ;; is this the right thing?  What does "DV" do?
;;  vc-vss-open-async t ;; what does this do?
;;  vc-vss-open-async-rw t) ;; ?

;; (add-to-list 'exec-path
;; 			 "c:/Program Files/Microsoft Visual Studio/VSS/Win32")

;; (add-to-list 'vc-handled-backends 'VSS)

;; ;;optional: these may be needed if you are not "logging into the domain"
;; (setenv "SSDIR" "\\\\sourcesafe\\someDatabase") ;directory of the ss.ini for that database
;; (getenv "PATH")
;; (setenv "SSUSER" "username") ;without domain
;; (setenv "SSPWD" "password")


;; source-safe.el is less integrated, but doesn't give perfomance hit
;; (setq ss-project-dirs '(("^c:\\\\home\\\\work" . "$/")))
;; (autoload 'ss-diff "source-safe" "" t)
;; (autoload 'ss-get "source-safe" "" t)
;; (autoload 'ss-checkout "source-safe" "" t)
;; (autoload 'ss-uncheckout "source-safe" "" t)
;; (autoload 'ss-checkin "source-safe" "" t)
;; (autoload 'ss-merge "source-safe" "" t)
;; (autoload 'ss-history "source-safe" "" t)
;; (autoload 'ss-status "source-safe" "" t)
;; (autoload 'ss-locate "source-safe" "" t)




;; Bookmarks
;;deletion often fails because the location is dependent upon the name (very dumb)
;;todo:  alternative implementation:  use point-to-register
(define-key global-map [(shift f2)] 'bookmark-delete-by-line)
(define-key global-map [(control f2)] 'bookmark-set-by-line)
(define-key global-map [(f2)] 'bookmark-get-next)
(define-key global-map [(control shift f2)] 'list-bookmarks)

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



(provide 'w32-dev)
;;; w32-dev.el ends here
