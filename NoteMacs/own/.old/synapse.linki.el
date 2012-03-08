;;; synapse.el --- Expose connections. -*- mode: emacs-lisp -*-

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Patrick Anderson (concat (nreverse (string-to-list ">gro.llehseerf@erawtap<")) nil)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;;; Theory:
;;  Ideas are connected.
;;  Text expresses ideas.
;;  Synapse exposes connections.


;;; Commentary:
;;  Ideas are linked together without special spelling or markup.
;;  Each idea is stored in a carefully named plain-text file.
;;  File names are used as tokens to construct the XHTML links.
;;  Additional regular expressions allow mostly WYSIAWYG editing.


;;; Installation:
;;  Try something like this in your .emacs:
;; (autoload 'synapse-mode ".../Emacs/site-lisp/synapse.el" "synapse" t)
;; (setq auto-mode-alist'
;; 	  (append
;; 	   '((".*/synapse/source/.*" . synapse-mode))
;; 	   auto-mode-alist))

;;  Now create some text documents:
;;    ~/synapse/source/todo
;;    ~/synapse/source/Swadeshi
;;    ~/synapse/source/comp/Free Software


;;; Usage:
;;  In `synapse-mode':  RET  or  `mouse-2'  on highlighted text visits that file.
;;  Main entry points are `synapse', `synapse-mode', `synapse-make-page'
;;  and `synapse-dired-make-marked-pages'.

;;  Make file names shorter or more generalized to clump ideas.
;;  Make file names longer or more specific to avoid unwanted/unexpected synapseng.


;;; Todo:
;;  ERROR: regular-expression-too-large after ~ 1500 documents!

;;  Check `synapse-output-dir'\<file>.htm timestamp in `synapse-make-page'
;; (defun synapse-file-stale? (file)
;;   (format-time-string
;;    "%a, %b %d %Y, %l:%M:%S %p"
;;       (nth 5 (file-attributes file)) t))

;; (nth 7 (file-attributes file)) ;;size

;;  REDIRECT synonyms?

;; CGI:
;;    `open-network-stream-server'
;;    The "| edit |" menu entry sends request.
;;    We reply with large <input> field and `Save' button.
;;    The `Save' button POSTs changes.
;;    `synapse-edit-page' calls "monotone commit <file>"
;;    then runs `linki-make-page'

;;    The "| new |" menu entry sends request.

;;  
;;  Performance increase
;;  try various prefixes and suffixes
;;    (un)lock(ed|ing|s|) clos(ed|ing|s|e)
;;    (blue|rasp|elder|logan)[ ]?berr(y|ies)
;;  synonyms, antonyms
;;  Cookie backed stylesheet selection.
;;  (concat "http://www.google.com/search?q=" token)
;;  Only tokenize whole words?  hmm... Maybe a thesaurus?
;;  Timestamp.  r&d: visited-file-modtime,
;;  Append backlinks.


;;; Version:
;; 8 Synapse
;;  Renamed to synapse.el
;;  Rework docs.
;;  Escape XML entities, begin markup rules _underline_ *bold* /italic/      

;; 7
;;  added ftp://, gopher:// and mail:// protocols
;;  Fix many markup oddities

;; 6
;;  Cleanup docs.
;;  Still no backlinks...

;; 5 Incremental make
;;  C-cC-c in `linki-mode' calls `linki-make-page'
;;  Integrate linki.cmd cruft
;;  fixed stylesheet to work with Internet Exploder
;;  `linki-dired-make-marked-pages'

;; 4 Fewer broken links.  Easy editing.
;;  Sort tokens by length to make Subtokens less precedent.
;;  `linki-mode'

;; 3 Fewer glitches
;;  Respect document capitalization.
;;  &nbsp; problems fixed.
;;  header and footer now integrated (not separate files)

;; 2 Usable.  Less slow.
;;  Menu at top instead of bottom of page.

;; 1 Mostly correct.  Much more complicated and much, much slower.

;; 0 Brute force (broken links).
;;  The existence of a file creates a token of that name.
;;  Each token in each document is then linkified.
;;  http:// recognized and linkified.


;;; Customizations:
;;  ../style/synapse.css
(defcustom synapse-output-dir "../htm" "where to write the output")


;;; Code:
(require 'interpreter-minor)

(defun synapse-recurse (process-file)
  (mapcar
   (lambda (token)
	 (if (not (file-directory-p token))
		 (funcall process-file token)))
   (sort (directory-files ".") ;; this makes tokens
		 (lambda (x y)
		   (< (length x) (length y)))))) ;; precedent over subtokens

(defvar synapse-mapping nil)
(defconst synapse-protocols
  "\\(?:http\\|ftp\\|gopher\\|mail\\):[^ \t\r\n]+"
  "supported protocols")

(defun synapse-build-mapping ()
  (setq synapse-mapping nil)
  (dolist (token
		   '(
			 ;;these are order dependent (last has highest priority)
			 ("\?" "\?")
			 ("!" "!")
			 ("\\\[" "[")
			 ("\\\]" "]")
			 ("(" "(")
			 (")" ")")
			 ("{" "{")
			 ("}" "}")
			 ("_" "_")
			 ("\\\*" "*")
			 ("`" "`")
 			 ("/" "/")
 			 ("\\\\" "\\\\")
			 ("-" "-")
			 ("+" "+")
			 ("=" "=")
			 (";" ";")
			 (":" ":")
			 ("\\\." "\.")

			 ("[\n\r]" " <br/>\n")
			 ("\t" "&nbsp;&nbsp;&nbsp;&nbsp;") ;;TAB
			 (" " " ")
			 ("  " " &nbsp;")
			 ("^ " "&nbsp;")
;;			 (synapse-protocols "<a href=\"\\&\">\\&</a>")))
			 ("\\(?:http\\|ftp\\|gopher\\|mail\\):[^ \t\r\n]+" "<a href=\"\\&\">\\&</a>")

			 ;;entities
			 ("<" "&lt;")
			 (">" "&gt;")
			 ("&" "&amp;")
			 ("'" "&apos;")
			 ("\"" "&quot;")
))
	(setq synapse-mapping (cons token synapse-mapping)))

  ;;the longest file name has the highest priority
  (synapse-recurse
   (lambda (token)
	 (setq synapse-mapping
		   (cons (list (regexp-quote token)
					   (concat "<a href=\"" token ".htm\">\\&</a>"))
				 synapse-mapping)))))

(defun synapse ()
  (interactive)
  (let ((synapse-started (current-time-string)))
	(synapse-gen-missing-image-files)
	(shell-command "ls -U -1 -t > \"recent changes\"")
	(shell-command "ls -R -U -1 > \"all pages\"")
	(synapse-build-mapping)
;;	(make-directory synapse-output-dir)
	(synapse-recurse 'synapse-make-page-internal)
;;make special pages such as synapse.htm
	(message (concat "synapse started at " synapse-started))
	(message (concat "synapse ended at " (current-time-string)))))

(defun synapse-make-page (file)
  (interactive "fFile: ")
  (setq file (file-name-nondirectory file))
  (synapse-build-mapping)
  (synapse-make-page-internal file))

(defun synapse-dired-make-marked-pages ()
  (interactive)
  (synapse-build-mapping)
  (dired-map-over-marks
   (synapse-make-page-internal
	(file-name-nondirectory (dired-get-filename nil t))) nil))

(defun synapse-make-page-internal (file)
  (let* ((outpath (concat synapse-output-dir "/" file))
		 (htm (concat outpath ".htm")))
	(progn
	  (copy-file file htm t)
	  (save-excursion
		(set-buffer (find-file-noselect htm))
		(setq case-fold-search t)
		(goto-char (point-min))
		(let ((token-count (length synapse-mapping)))
		  (while (< (point) (point-max)) ;;while not EOF
			(let ((cur-point (point))
				  (max-point (point-max))
				  (cur-token 0)
				  (found nil))

			  (while (< cur-token token-count) ;;while more tokens to consider
				(let ((looking-at-token (car (nth cur-token synapse-mapping))))
				  (if (looking-at looking-at-token)
					  (progn ;;found the token

						;;if `cur-token' represents a file (not just a regexp)
						;;  then add `file' to `cur-token's backlink list
						;;after `synapse-make-pages' is complete, iterate through all files again, appending all found backlinks

						;;use assoc lists?
						;;  						   (defvar synapse-backlinks nil)
						;; 						   (setq synapse-backlinks
						;; 								 '(("some doc" "Austrian Pine" "Red Pine")
						;; 								   ("another doc" "Pitch Pine")
						;; 								   ("some other doc" "White Pine")))

						;; 						   (cons
						;; 							"black pine"
						;; 							(cdr (assoc-ignore-case "SOME doc" synapse-backlinks)))

						(replace-match (car (cdr (nth cur-token synapse-mapping))) t nil)	;;replace with markup
						(setq cur-token token-count) ;;break out of while
						(setq found t)
						;;`re-search-forward' already set the point to the end of the replacement text
						)
					(setq cur-token (+ 1 cur-token))) ;;didn't find a match, so try the next token at the same point
				  ))

			  (if (not found) ;;if no match was found
				  (progn
					(while (and (< (point) max-point) ;;while not at EOF
								(looking-at "[^ 	\n\r]")) ;;until end of this token
					  (forward-char))))))


		  ;;header
		  (goto-char (point-min))
		  (insert
		   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		   "\n"
		   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
		   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
		   "\n"
		   "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
		   "\n"
		   "<head>\n"
		   "	<link href=\"../style/synapse.css\" type=\"text/css\" rel=\"stylesheet\"/>\n"
		   "	<title></title>\n"
		   "</head>\n"
		   "\n"
		   "<body>\n")


		  ;;menu
		  (insert (concat
				   "<em>\n"
				   "  <a href=\"../../index.htm\">home</a> | "
				   "  <a href=\"recent changes.htm\">recent changes</a> | "
				   "  <a href=\"../source/" file "\">source</a> | "
				   "  <a href=\"synapse?action=edit;id="
				   (replace-regexp-in-string
					" " "%20" file nil t)
				   "\">edit</a> | "

				   "<input type=\"text\" name=\"new\" maxLength=\"256\" value=\"\" size=\"20\"/> "
				   "<input type=\"submit\" name=\"newButton\" disabled=\"disabled\" value=\"new\"/><br/>\n"

 				   "</em>\n"
				   "<br/>\n"
				   ))


		  ;;images
		  (if (file-exists-p
			   (concat "../images/" file ".jpg"))
			  (insert (concat "<img src=\"../images/" file ".jpg\" alt=\"" file "\"/><br/>")))

		  (if (file-exists-p
			   (concat "../images/" file ".gif"))
			  (insert (concat "<img src=\"../images/" file ".gif\" alt=\"" file "\"/><br/>")))


		  ;;footer
		  (goto-char (point-max))
		  (insert
		   "<div align=\"right\">\n"
		   "  <em>\n"
		   "    made with <a href=\"synapse.htm\" >synapse</a>\n"
		   "  </em>\n"
		   "</div>\n"
		   "</body>\n"
		   "</html>\n")

		  (save-buffer)
		  (kill-buffer (current-buffer))
		  )))))

;;run in source dir to generate placeholders for image files
(defun synapse-gen-missing-image-files ()
  (interactive)
  (mapcar
   (lambda (file)
	 (let ((f (file-name-sans-extension file)))
	   (if (not (file-exists-p f))
		   (progn
			 (find-file f)
			 (insert " ")
			 (save-buffer)
			 (kill-buffer (current-buffer))))))
   (directory-files "../images")))


;;synapse-mode
(defvar synapse-font-lock nil)
(defun synapse-build-font-lock ()
  "fill `synapse-font-lock' with all tokens"
  (setq synapse-font-lock nil)
  (synapse-recurse
   (lambda (token)
	 (setq synapse-font-lock
		   (concat (regexp-quote token) "\\|" synapse-font-lock)))) ;;fill with filenames

  (setq synapse-font-lock
		(concat "\\(\\)\\(" synapse-protocols "\\|" synapse-font-lock))
  (setq synapse-font-lock
		(concat synapse-font-lock "zzzzzzzz\\)")))

(defun synapse-follow ()
  (save-excursion
	(re-search-forward synapse-font-lock nil t))
  (ffap (match-string 2)))

(defun synapse-mode-rebuild-mode ()
  (interactive)
  (synapse-build-font-lock)
  (im-make-mode synapse "synapse --- Exposing connections." synapse-font-lock synapse-follow)
  (synapse-mode))

(synapse-mode-rebuild-mode)

(define-key synapse-mode-map [(f5)] 'synapse-mode-rebuild-mode)

(defvar synapse-C-c-map (make-sparse-keymap))
(define-key synapse-mode-map [(control c)] synapse-C-c-map)
(define-key synapse-C-c-map [(control c)] 
  (lambda () (interactive)
	(synapse-make-page buffer-file-name)))

(provide 'synapse)
;;; synapse.el ends here
