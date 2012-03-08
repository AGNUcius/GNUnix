;;; findr.el -- Breadth-first file-finding facility for (X)Emacs
;;  Thursday July 30 1999

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@bu.edu>
;; Version: 0.9pre
;; Created: Tue Jul 27 12:49:22 EST 1999
;; Keywords: files

;; This file is not part of Emacs or XEmacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.


;;; Commentary:

;; This code contains a command, called `findr', which allows you to
;; search for a file breadth-first.  This works on UNIX, Windows, and
;; over the network, using efs and ange-ftp. It's pretty quick, and (at
;; times) is a better and easier alternative to other mechanisms of
;; finding nested files, when you've forgotten where they are.

;; You pass `findr' a regexp, which must match the file you're looking
;; for, and a directory, and then it just does its thing:

;; M-x findr <ENTER> ^my-lib.p[lm]$ <ENTER> c:/ <ENTER>

;; If called interactively, findr will prompt the user for opening the
;; found file(s).  Regardless, it will continue to search, until
;; either the search is complete or the user quits the search.
;; Regardless of the exit (natural or user-invoked), a findr will
;; return a list of found matches.


;; Entrypoints and .emacs setup suggestion:
;; (autoload 'findr "findr" "Find file name." t)
;; (define-key global-map [(meta control S)] 'findr)

;; (autoload 'findr-search "findr" "Find text in files." t)
;; (define-key global-map [(meta control s)] 'findr-search)

;; (autoload 'findr-query-replace "findr" "Replace text in files." t)
;; (define-key global-map [(meta control r)] 'findr-query-replace)


;; Change Log:

;; 0.1: Added prompt to open file, if uses so chooses, following
;;      request and code example from Thomas Plass.
;; 0.2: Made `findr' not stop after the first match, based on the
;;      request by Thomas Plass.
;;      Also, fixed a minor bug where findr was finding additional
;;      files that were not correct matches, based on
;;      `file-relative-name' misuse (I had to add the 2nd arg to it).
;; 0.3: Added a `sit-for' for redisplay reasons.
;;      Modifications as suggested by RMS: e.g. docstring.
;; 0.4  Added `findr-query-replace', courtesy of Dan Nelsen.
;; 0.5  Fixed spelling and minor bug in `findr-query-replace' when
;;      non-byte-compiled.
;; 0.6  http://groups.google.com/groups?selm=cxjhfml4b2c.fsf_-_%40acs5.bu.edu :
;; From: David Bakhash (cadet@bu.edu)
;; Subject: Re: latest version of findr.el (5)
;; This is the only article in this thread
;; View: Original Format
;; Newsgroups: gnu.emacs.sources
;; Date: 1999/07/31
;; Courtesy of Dan Nelsen, this version has search-and-replace capabilities.
;; it's still a bit experimental, so I wouldn't expect too much of it.  But it
;; hasn't been tested yet for friendly behavior.

;; The function `findr-query-replace' wasn't working unless you byte-compile the
;; file.  But, since findr is really designed for speed, that's not a bad thing
;; (i.e. it forces you to byte-compile it).  It's as simple as:

;; M-x byte-compile-file <ENTER> /path/to/findr.el <ENTER>

;; anyhow, I think it should work now.

;; dave

;; ---- the next few versions are from Patrick Anderson:
;; 0.7 Added `findr-search', broke `findr'
;;   This was the first version held at emacswiki.org

;; 0.8 fixed 0.7 breakage.

;; 0.9pre
;;   `findr-dired' like `find-dired'?
;;   `findr-search' like `grep-find'?
;;   `findr-replace' like `grep-edit'? or `moccur-edit'? or globrep.el?

;; Code:
(eval-when-compile
  (require 'cl))

(defvar findr-mode-map (make-sparse-keymap))
(define-key findr-mode-map [(n)] 'pan-up)
(define-key findr-mode-map [(down)] 'pan-up)
(define-key findr-mode-map [(p)] 'pan-down)
(define-key findr-mode-map [(up)] 'pan-down)
;; (define-key findr-mode-map [(g)] 'findr-revert)

;; (defun findr-revert ()
;;   (interactive))
;; ;;   (findr findr-url)
;; ;;   (setq findr-history (cdr findr-history)))

;; (define-key findr-mode-map [(9)] 'findr-next)
;; (defun findr-next ()
;;   "jump to next chunk"
;;   (interactive)
;; ;;   (re-search-forward findr-href)
;; ;;   (message (match-string 1)))
;; )

;; (define-key findr-mode-map [(shift 9)] 'findr-prev)
;; (defun findr-prev ()
;;   "jump to prev chunk"
;;   (interactive)
;;   (re-search-backward findr-href)
;;   (message (match-string 1)))

;; (define-key findr-mode-map [(13)] 'findr-follow)
;; (defun findr-follow ()
;;   "follow this link"
;;   (interactive)
;;   (re-search-backward findr-href)
;;   (findr-parse-url (replace-regexp-in-string "%20" " " (match-string 1))))

;; (define-key findr-mode-map [(mouse-2)] 'findr-follow-mouse)
;; (defun findr-follow-mouse (event &optional univ)
;;   (interactive "e\nP")
;;   (goto-char (posn-point (event-end event)))
;;   (findr-follow))

(define-key findr-mode-map [(q)] 'bury-buffer)

;; (defvar findr-font-lock-keywords
;; (setq findr-font-lock-keywords
;;   (list
;;    '("^\\(.*?\\):\\(.*?\\):"
;;      (1 font-lock-preprocessor-face) (2 font-lock-variable-name-face))))

(defun findr-mode ()
  "View, possibly change text across files. \\{findr-mode-map}"
  (interactive)
  (kill-all-local-variables)
  ;; (setq buffer-read-only t)
  ;;  (use-local-map findr-mode-map)
  (setq mode-name "findr")
  (setq major-mode 'findr-mode))
;;   (make-local-variable 'font-lock-defaults)
;;   (setq font-lock-defaults '(findr-font-lock-keywords t t nil nil)))


(defun* findr (name dir &key (prompt-p (interactive-p)))
  "Search directory DIR breadth-first for files matching regexp NAME.
If PROMPT-P is non-nil, or if called interactively, Prompts for visiting
search result\(s\). \\{findr-mode-map}"
  (interactive "sfile name \(regexp\): \nDDirectory: ")
  (let ((*dirs* (findr-make-queue))
		buf
		*found-files*)

	(if prompt-p
		(setq buf (switch-to-buffer "*findr*")))

	;;	(erase-buffer)
    (labels ((findr-1 (dir)
					  (message "searching %s ..." dir)
					  (let ((files (directory-files dir t "\\w"))
							(first-occurence t))

						(loop

						 ;; -*- mode: python -*-
						 for file in files
						 for fname = (file-relative-name file dir)
						 when (file-directory-p file)
						 do (findr-enqueue file *dirs*)

						 when (string-match name fname)
						 do
						 (progn
						   (if (and prompt-p first-occurence)
							   (progn
								 (insert (concat "  " (file-name-as-directory dir) ":\n"
												 "  total 25k\n"
												 "  drwxrwxrwx    0 Dec  5 13:20 .\n"
												 "  drwxrwxrwx    0 Dec  4 21:07 ..\n"))
								 (setq first-occurence nil)))

						   (if (file-regular-p file)
							   (progn
								 (push file *found-files*)

								 (if prompt-p
									 (insert (concat "  -rw-rw-rw- 7.0k Dec  5 13:20 " fname "\n")))
								 )

							 ;;else, a dir?
							 (if prompt-p
								 (insert (concat "  drwxrwxrwx    0 Dec  4 21:07 " fname "\n")))
							 )
						   )))))

	  ;; -*- mode: emacs-lisp -*-
      (unwind-protect
		  (progn
			(findr-enqueue dir *dirs*)
			(while (findr-queue-contents *dirs*)
			  (findr-1 (findr-dequeue *dirs*)))
			(message "searching ... done")
			(if prompt-p
				(dired-virtual-mode)))

		;; protected:
		(return-from findr (nreverse *found-files*))))))

(defalias 'findr-dired 'findr)

(defun findr-query-replace (from to file dir)
  "Do `query-replace-regexp' of FROM with TO, on each file found by findr."
  (interactive
   "sReplace text through files: (text regexp): \nsReplace %s by: \nsLook in these files: (files regexp): \nDStart in directory: ")
  (tags-query-replace from to nil '(findr file dir)))


(defun findr-search (regexp files dir &optional add)
  "Recursive search for regexp through all files in dir"
  (interactive
   "sSearch through files for: (text regexp): \nsLook in these files: (files regexp): \nDStart in directory: ")
  ;;  (tags-search regexp '(findr files dir)))
  (save-excursion
	(save-window-excursion
	  (kill-buffer (get-buffer "*findr-search*"))
	  (let ((buf (get-buffer-create "*findr-search*")))

		;; 	  (toggle-read-only -1)
		;; 	  ;;get rid of `compilation-mode' read-only so we can erase the buffer?
		;; 	  (set-text-properties (point-min) (point-max) nil)

		;; 	  (if add
		;; 		  (end-of-buffer)
		;; 		(erase-buffer))

		(mapcar
		 (lambda (file)
		   (let ((openp (find-buffer-visiting file))
				 (fname (file-relative-name file dir)))

			 (if openp
				 (set-buffer openp)
			   (find-file-literally file))

			 (beginning-of-buffer)
			 (while (re-search-forward regexp nil t)
			   (progn
				 (beginning-of-line)
				 (let ((beg (point))
					   (line (line-number-at-pos)))
				   (forward-line)
				   (copy-to-register ?z beg (point))

				   (with-current-buffer buf
					 (let ((beg (point))
						   (mid)
						   (end))

					   (insert (concat
								fname
								":"))

					   (setq mid (point))

					   (put-text-property
						beg mid
						'face 'font-lock-constant-face)

					   (insert (concat
								(number-to-string line)
								":"))

					   (setq end (point))

					   (add-text-properties
						beg end
						'(mouse-face 'highlight))

					   (add-text-properties
						beg end
						'(keymap 'findr-mode-map))

					   (add-text-properties
						beg end
						'(help-echo "mouse-2: visit this file in other window"))

					   (insert-register ?z t)

					   (add-text-properties
						beg end
						'(intangible t))

					   (put-text-property
						beg end
						'read-only t)

					   )))))
			 (if (not openp)
				 (progn
				   (print (current-buffer))
				   (kill-buffer (current-buffer))))))

		 (findr files dir))

		(with-current-buffer buf
		  (compilation-mode)
		  (beginning-of-buffer)))
	  )))

;; 				 (overlay-put (make-overlay mid end) 'face 'font-lock-function-face)
;; 				 (overlay-put (make-overlay beg mid) 'face 'font-lock-constant-face)



;;;; Queues

(defun findr-make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)
    q))

(defun findr-enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun findr-dequeue (q)
  "Remove an item from the front of the queue."
  (prog1 (pop (cdr q))
    (when (null (cdr q))
      (setf (car q) q))))

(defsubst findr-queue-contents (q)
  (cdr q))

(provide 'findr)
