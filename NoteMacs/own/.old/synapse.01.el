;;; synapse.el --- Expose idea connections. -*- mode: emacs-lisp -*-

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
;;  Tokens are sorted by length so longer tokens take precedence.


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

;;  Markup rules: _underline_ *bold* /italic/

;;  C-cC-c in `synapse-mode' calls `synapse-make-page'


;;; Todo:
;;  BUG: regular-expression-too-large after ~ 1500 documents!

;;  Check `synapse-output-dir'\<file>.htm timestamp in `synapse-make-page'
;; (defun synapse-file-stale? (file)
;;   (format-time-string
;;    "%a, %b %d %Y, %l:%M:%S %p"
;;       (nth 5 (file-attributes file)) t))
;; (nth 7 (file-attributes file)) ;;size

;; CGI:
;;    `open-network-stream-server'
;;    The "| edit |" menu entry sends request.
;;    We reply with large <input> field and `Save' button.
;;    The `Save' button POSTs changes.
;;    `synapse-edit-page' calls "monotone commit <file>"
;;    then runs `synapse-make-page'

;;    The "| new |" menu entry sends request.

;;  Increase performance 
;;  Try various prefixes and suffixes
;;    (un)lock(ed|ing|s|) clos(ed|ing|s|e)
;;    (blue|rasp|elder|logan)[ ]?berr(y|ies)
;;  Synonyms, antonyms
;;  Cookie backed stylesheet selection.
;;  (concat "http://www.google.com/search?q=" token)
;;  Only tokenize whole words?  hmm... Maybe a thesaurus?
;;  Append backlinks.

;;; Version:
;; .01 new

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

			 ;;XML entities
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
  (let ((synapse-started (current-time)))
	(synapse-gen-missing-image-files)
	(shell-command "ls -U -1 -t > \"recent changes\"")
	(shell-command "ls -U -1 -R > \"all pages\"")
	(synapse-build-mapping)

	(if (not (file-exists-p synapse-output-dir)
			 (make-directory synapse-output-dir)))

	(synapse-recurse 'synapse-make-page-internal)

;;make special pages such as synapse.htm

;; 	(if (file-exists-p "synapse.el")
;; 		(progn
;; 		  (htmlize-file "synapse.el" synapse-output-dir)
;; 		  (rename-file )

	(message
	 (concat "synapse finished in "
			 (format-time-string
			  "%M minutes, %S seconds"
			  (time-subtract (current-time) synapse-started))))))

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
					  (forward-char))))


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
		   "    Made with <a href=\"../source/synapse.el\" >synapse</a>\n"
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
