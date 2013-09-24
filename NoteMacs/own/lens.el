;;; lens.el --- Focus on text. -*- emacs-lisp -*-

;; Copyright (C) 2012->oo  Social Sufficiency Coalition

;;  This program is free software: you can
;;  redistribute it and/or modify it under the
;;  terms of the GNU Affero General Public License
;;  as published by the Free Software Foundation,
;;  either version 3 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that
;;  it will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.  See the GNU Affero General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU
;;  Affero General Public License along with this
;;  program.  If not, see
;;  http://www.GNU.org/licenses

;;; FEATURES
;; autolink plain-text documents

;;; INSTALLATION
;; Try something like this in your .emacs:

;; (autoload 'lens-mode "~/lens/.code/lens.el" "lens" t)
;; (setq auto-mode-alist'
;;    (append
;;     '((".lens" . lens-mode))
;;     auto-mode-alist))

;; Now create a .lens file such as:


;;  Main entry points are `lens', `lens-mode', `lens-make-page'
;;  `lens-dired-make-marked-pages' and `lens-clean'

;;  In `lens-mode': RET or `mouse-2' at beginning of term visits that file.

;; `lens-mode' keys:
;; f5       `lens-mode-rebuild-mode'
;; C-cC-c   `lens-make-page'
;; C-cC-p   preview generated HTML of this source file
;; RET      Follow link
;; C-m      Insert line-feed (\n)


;;; FORMATTING
;; Whitespace is preserved.

;; At beginning of line (BOL)
;;  = Header1
;;  == Header2
;;  === Header3
;;  ==== Header4
;;  $ shell command
;;  : bullet
;;  * bullet
;;  + bullet
;;  . bullet
;;  > quote

;; Anywhere
;;  >> quote2
;;  >>> quote3
;;  "'quote'"
;;  /* comment */
;;  ;; comment
;;  (parenthetic)
;;  {parenthetic}
;;  [parenthetic]
;;  \\UNC-Server\Shared_Folder
;;  "\\UNC-Server\Shared Folder"
;;  ~/LocalFile
;;  ./LocalFile
;;  ../LocalFile

;; Implicit URLs: `lens-implicit-HTTP'
;; Explicit URLs: [file|https?|ftp]://


;;; VERSION:
;; .01 new

;;; CUSTOMIZATIONS:
(defcustom lens-global-page-title nil "Title of _all_ generated pages.  If set to `nil' the title is the name of the input file.")
(defcustom lens-output-dir "." "where to write the output")
(defcustom lens-img-dir ".data" "where <img> content is located")
(defcustom lens-default-stylesheet ".code/preferred.css" "stylesheet")

(defcustom lens-host-mail "AGNUcius@Gmail.com" "mail address to send edits")

(defcustom lens-shortest-inner
  ;; 3 ;to many inner matches
  4 ;not enough prefix coverage
  "Shortest term to match _within_ other terms.
All terms less than this match only at the beginning of words (using `\\b')")

(defcustom lens-encoding "UTF-8" ;"ISO-8859-1"
  "XML encoding attribute")


;;; CODE:
(require 'time-date)
(require 'interpreter-minor)

;; The following effect only `font-lock'
;; KLUDGE: see `lens-build-mapping' for HTML regexps

;; (defconst lens-local-*nix-path
;;   ;; ../ or ./ followed by anything but |, >, or whitespace
;; ;;  "\\(\\.\\./\\|\\./\\|/\\)[^|> \t\n\r]*"
;; ;;"\\(\\.\\./\\|\\./\\)[^|> \t\n\r]*"
;;   "/[^]):,;? \t\n\r]+"
;;   "Path on local hard drive")

(defconst lens-quoted-local-M$-path
  "\"\\([a-zA-Z]:\\\\.+?\\)\""
  "Quoted path on local hard drive")

(defconst lens-local-M$-path
  ;;"\\\\[^\\\\].+?[^]):,;? \t\n\r]+"
  "[a-zA-Z]:\\\\[^]):,;? \t\n\r]+"
  "Path on local hard drive")

(defconst lens-quoted-UNC-path
  "\"\\(\\\\\\\\[^]:?]+?\\)\""
  "Quoted UNC")

(defconst lens-UNC-path
  "\\\\\\\\[^]):,;? \t\n\r]+"
  "UNC path on MS network")

(defconst lens-explicit-URL
;;  "\\(file\\|ftp\\|https?\\)://[^ \t\n\r]+"
  "\\([a-zA-Z0-9]\\)+://[^ \t\n\r]*"
  "Specify protocol to include chars banned from implicit-HTTP.")

(defconst lens-implicit-HTTP
;;   "\\([a-zA-Z][a-zA-Z0-9_-]*\\.[a-zA-Z0-9-_]+\\)[^])}>:,; \t\n\r]+"
  "\\(\\([a-zA-Z0-9_-]\\)+\\.\\)+\\(aero\\|am\\|at\\|au\\|be\\|biz\\|ca\\|cat\\|cc\\|ch\\|com\\|coop\\|cx\\|cz\\|da\\|de\\|dk\\|edu\\|es\\|eu\\|fi\\|fr\\|gov\\|hu\\|ie\\|il\\|im\\|in\\|it\\|info\\|int\\|jp\\|jobs\\|lt\\|me\\|mil\\|mobi\\|museum\\|name\\|net\\|nl\\|no\\|nu\\|nz\\|org\\|pl\\|pro\\|pt\\|ro\\|ru\\|se\\|si\\|sk\\|tel\\|to\\|travel\\|tv\\|uk\\|us\\|ws\\|za\\)[^])}>:,; \t\n\r]*"
;;maybe use `regexp-opt' here?
  "Characters ])}>:,; \\t\\r\\n end the implicit HTTP URL.")

(defconst lens-index.htm
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
 <head>
   <title></title>
   <meta http-equiv=\"REFRESH\" content=\"0; URL=_home.htm\"/>
 </head>
<body/>
</html>"
"Redirect to real start page")

(defvar lens-css
  "
body
{
	font-family:monospace;
	font-size:120%;
}

#main {padding-left:4px;}
#header {border-bottom:solid; font-family:arial, san-serif;}
#footer {border-top:solid; font-family:arial, san-serif; font-size:50%;}

span.bullet{margin-left:1%;font-size:150%;}
span.indent{margin-left:1%;}

span.quot,span.quot2,span.quot3{
	font-weight:normal;
	font-style:italic;
	font-size:100%;
}

span.italic{font-style:italic;}
span.bold{font-weight:bold;}

span.h1, span.h2, span.h3, span.h4 {font-family:arial, san-serif; background:#ddd;}
span.h1{font-size:300%;}
span.h2{font-size:250%;}
span.h3{font-size:200%;}
span.h4{font-size:150%;}

a{text-decoration:none;}
a:visited{text-decoration:none;}
a:hover,a:focus{text-decoration:none;}

input{font-size:65%;}
img{float:right;}

span.rel{background:#070; color:#fff;}
span.date{background:#000; color:#fff;}

span.quot{background:#ffe;}
span.quot2{background:#eff;}
span.quot3{background:#fef;}

span.shell{background:#ddd;}
span.cmnt{background:#dfd;}
span.type{background:#9bf;}

a{color:#060;}
a:visited{color:#606;}
a.ext{color:#77f;}
a:hover, a:focus{background:#006; color:#ff0;}
a.ext:hover, a.ext:focus{background:#77f; color:#ff0;}
"
"embeded css")





(defvar lens-mapping nil)
(defvar lens-input-dirs '("."))
(defvar lens-closing nil)

(defun lens-shortest-inner (term)
  (if (< (length term) lens-shortest-inner)
	  (if (<= (length term) 2)
		  (concat "\\b" (regexp-quote term) "\\b") ;;find tiny terms only if they stand alone
		(concat "\\b" (regexp-quote term))) ;;find short terms only at beginning of word
	(regexp-quote term)))					 ;;Term otherwise

;;This doesn't really recurse yet, but it really should be a collection of files and directories to allow posting of .files etc.
(defun lens-recurse (process-file)
  (mapcar
   (lambda (term)
     (if (not (file-directory-p term))
         (funcall process-file term)))
   ;; longer terms take precedence
   (sort (directory-files ".")
         (lambda (x y)
           (< (length x) (length y))))))

(defun lens-clean ()
  "like \"make clean\""
  (interactive)
  (shell-command (concat "rm " lens-output-dir "/*.htm")))

(defun lens-build-mapping ()
  (setq lens-mapping nil)

  ;;the longest file name has the highest priority
  ;;  (lens-recurse (file-name-directory (expand-file-name "."))
  (lens-recurse
   (lambda (file)
     (setq lens-mapping
           (cons
			(list
			 (lens-shortest-inner file)
			 (concat
			  "<a href=\"" file ".htm"
;; 			  (if (file-name-extension file)
;; 				  nil
;; 				".htm")
				   "\">\\&</a>"))
                 lens-mapping))))

  (dolist
	  (term
	   '(
		 ;;The first entry here has LOWEST priority.
		 ;;must appear at BOL
;;   		 ("^.[^ \n\r\t]+?:" "<span class=\"type\">\\&" "</span>")

;;  		 ("^.*?:" "<span class=\"h4\">\\&" "</span>");anything followed by a : is a title?

 		 ;;see `lens-*nix-path'
;; 		 ("\\(/\\|\./\\|\.\./\\)[^]):,;? \t\n\r]+"
;; 		  "<a class=\"ext\" href=\"file:///\\&\">\\&</a>")

 		 ("^Related:" "<span class=\"rel\">\\&</span>")
		 ;;("^Related:" "<span class=\"rel\">\\&" "</span>")
 		 ("^[a-z]\\{3\\}-[0-9]\\{1,2\\}-[0-9]\\{2,4\\}:" "<hr/><span class=\"date\">\\&</span>")
 		 ("^>" "<span class=\"quot\">\\&" "</span>")

 		 ("^=" "<span class=\"h1\">\\&" "</span>")
 		 ("^==" "<span class=\"h2\">\\&" "</span>")
 		 ("^===" "<span class=\"h3\">\\&" "</span>")
 		 ("^====" "<span class=\"h4\">\\&" "</span>")

 		 ("^\\$ " "<span class=\"shell\">\\&" "</span>")

		 ;;("^[ \t]*\\([0-9]\\|[A-Z]\\|[a-z]\\)+\\(\\.\\|:\\)$" "<span class=\"bullet\">\\&</span>")

 		 ("^[ \t]*\\(:\\|\\*\\|\\+\\|\\.\\)" "<span class=\"bullet\">\\&</span>")
;; 		 ("^[ \t]*:" "<span class=\"indent\">\\&</span>")

 		 ("^::" "<span class=\"cmnt\">\\&" "</span>") ;;Batch-file comments
;;  		 ("^REM" "<span class=\"cmnt\">\\&" "</span>") ;;Batch-file comments

 		 ("(\\|{\\|\\[" "<small>\\&")
 		 (")\\|\\}\\|]" "\\&</small>")

 		 ("\"'" "<span class=\"quot\">\\&")
 		 ("'\"" "\\&</span>")

		 ;;This is not correct.  We should emit to align at column mod(4)
		 ;;Wikipedia.org/wiki/Tab_key#Tabs_in_HTML
		 ("\t" "&nbsp;&nbsp;&nbsp;&nbsp;") ;;TAB
;; 		 ("\t" "\t") ;;doesn't align
;; 		 ("\t" "&#09;") ;;same as \t
;; 		 ("\t" "&#11;") ;;disallowed in SGML (HTML) and XML 1.0

		 ("  " "&nbsp;&nbsp;")
		 ("^ " "&nbsp;")

		 ;;remote images
  		 (";;" "<span class=\"cmnt\">\\&" "</span>")
 		 (">>" "<span class=\"quot2\">\\&" "</span>")
 		 (">>>" "<span class=\"quot3\">\\&" "</span>")

		 ;;KLUDGE: how do I expand these vars here?
		 ;;see `lens-quoted-local-M$-path'
		 ("\"\\([a-zA-Z]:\\\\.+?\\)\""
		  "<a class=\"ext\" href=\"file:///\\1\">\\&</a>")

;; 		 ;;see `lens-local-M$-path'
		 ("[a-zA-Z]:\\\\[^]):,;? \t\n\r]+"
		  "<a class=\"ext\" href=\"file:///\\&\">\\&</a>")

		 ;;see `lens-quoted-UNC-path'
		 ("\"\\(\\\\\\\\[^]:?]+?\\)\""
		  "<a class=\"ext\" href=\"file:///\\1\">\\&</a>")

		 ;;see `lens-UNC-path'
		 ("\\\\\\\\[^]):,;? \t\n\r]+"
		  "<a class=\"ext\" href=\"file:///\\&\">\\&</a>")

		 ;;TODO: encode & to &amp;
		 ;;see `lens-explicit-URL'
		 ("\\([a-zA-Z0-9]\\)+://[^ \t\n\r]*"
		  "<a class=\"ext\" href=\"\\&\">\\&</a>")

		 ;;see `lens-implicit-HTTP'
		 ("\\(\\([a-zA-Z0-9_-]\\)+\\.\\)+\\(aero\\|at\\|au\\|be\\|biz\\|ca\\|cat\\|cc\\|ch\\|com\\|coop\\|cx\\|cz\\|da\\|de\\|dk\\|edu\\|es\\|eu\\|fi\\|fr\\|gov\\|hu\\|ie\\|il\\|im\\|in\\|it\\|info\\|int\\|jp\\|jobs\\|lt\\|me\\|mil\\|mobi\\|museum\\|name\\|net\\|nl\\|nu\\|nz\\|org\\|pl\\|pro\\|pt\\|ro\\|ru\\|se\\|si\\|sk\\|tel\\|to\\|travel\\|tv\\|uk\\|us\\|ws\\|za\\)[^])}>:,; \t\n\r]*"
		  "<a class=\"ext\" href=\"http://\\&\">\\&</a>")

		 ;;XML entities
		 ("<" "&lt;")
		 ("&" "&amp;")

 		 ("<i>" "&lt;i><span class=\"itlc\">")
 		 ("</i>" "</span>&lt;/i>")

 		 ("<b>" "&lt;b><span class=\"bold\">")
 		 ("</b>" "</span>&lt;/b>")

		 ;;whitespace is now preserved in preferred.css/white-space:pre-wrap
		 ("\n" "<br/>\n")
		 ("\r" "")


;;;;;;;; ---- ADD TERMS ABOVE THIS LINE ---- ;;;;;;;;
;;;;;;;; Note: Entries at the BOTTOM of this list have HIGHEST priority
		 ))
    (setq lens-mapping (cons term lens-mapping))))

(defun lens ()
  (interactive)
  (let ((lens-started (current-time)))
    (lens-gen-missing-image-files)
    (shell-command "ls -1 > _index") ;;BUGBUG
    (lens-build-mapping)

    (if (not (file-exists-p lens-output-dir))
        (make-directory lens-output-dir))

    (lens-recurse
	 'lens-make-page-internal)

  (let ((new-file (concat lens-output-dir "/index.htm")))
	(set-buffer (find-file-literally new-file))
	(insert-string lens-index.htm)
	(copy-file new-file (concat new-file "l") t))

    (message
     (concat "lens started at "
             (format-time-string
              "%I:%M:%S \n"
			  lens-started)

			 "and finished at "
             (format-time-string
              "%I:%M:%S"
              (current-time) )))))


(defun lens-make-page (file)
  (interactive "fFile: ")
  (setq file (file-name-nondirectory file))
  (lens-build-mapping)
  (lens-make-page-internal file))

(defun lens-dired-make-marked-pages ()
  (interactive)
  (lens-build-mapping)
  (dired-map-over-marks
   (lens-make-page-internal
    (file-name-nondirectory (dired-get-filename nil t))) nil))

(defun lens-make-page-internal (file)
  (let ((new-file (concat lens-output-dir "/" file))
        (process ".htm")
		;; 		 (if (file-name-extension file)
		;;                      nil
		;;                    ".html"))
		(modified (format-time-string "%B %e, %Y %l:%M %p" (nth 5 (file-attributes file)))))

    (setq new-file (concat new-file process))

    (if (file-newer-than-file-p new-file file)
        (message (concat new-file " is up to date"))

      (copy-file file new-file t)

      (if process
          (let ((found nil))
            (save-excursion)
            (set-buffer (find-file-literally new-file))
;;			(setq buffer-file-coding-system default-buffer-file-coding-system)
            (setq case-fold-search t)
            (goto-char (point-min))
            (let ((term-count (length lens-mapping)))
              (while (< (point) (point-max)) ;;while not EOF
                (let ((cur-point (point))
                      (max-point (point-max))
                      (cur-term 0)
                      (found nil))

                  (while (< cur-term term-count) ;;while more terms to consider
                    (let ((regexp (car (nth cur-term lens-mapping)))
						  (replacement (car (cdr (nth cur-term lens-mapping))))
						  (closing (cdr (cdr (nth cur-term lens-mapping)))))
                      (if (looking-at regexp)
                          (progn ;;found the term

;;if `cur-term' represents a file (not just a regexp)
;;  then add `file' to `cur-term's backlink list
;;after `lens-make-pages' is complete, iterate through all files again, appending all found backlinks

							;;if this map has a closing tag
							(if closing
								;;prepended to the current string so it closes first
								(setq lens-closing (concat (car closing) lens-closing)))

							(if (and (string= regexp "\n") ;if at EOL,
									 lens-closing);and there are tags to close,
								(progn
								  ;;close tags and follow with \n replacement
								  ;;(insert lens-closing replacement)
								  (insert lens-closing)
								  (setq lens-closing nil);empty our bucket
								  )
							  ;;else, just replace as normal
							  (replace-match replacement t nil))

                            (setq cur-term term-count) ;;break out of while
                            (setq found t)
;;`re-search-forward' already set the point to the end of the replacement text
                            )
                        (setq cur-term (+ 1 cur-term))) ;;didn't find a match, so try the next term at the same point
                      ))

                  (if (not found) ;;if no match was found
                      (forward-char))))


;;header
              (goto-char (point-min))
              (insert
			   "<?xml version=\"1.0\" encoding=\"" lens-encoding "\"?>\n"
               "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
               "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
               "\n"
               "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n"
               "\n"
               "<head>\n"
			   "  <title>"
			   (if lens-global-page-title
				   lens-global-page-title
				 file)
			   "</title>\n"
			   " <style type=\"css\">\n"
			   lens-css
			   " </style>\n"

               "</head>\n"
               "\n"
               "<body>\n"
			   )

;;top menu
              (insert
			   (concat
				"<div id=\"header\">\n"
				"<a href=\"_home.htm\">Home</a> |"
				;; " <a href=\"_news.htm\">News</a> |"
				 " <a href=\"_faq.htm\">FAQ</a> |"
				 " <a href=\"_thesis.htm\">Thesis</a> |"
				 " <a href=\"_diary.htm\">Diary</a> |"
				" <a href=\"_projects.htm\">Projects</a> |"
;;				" <a href=\"game.htm\">Game</a> |"
;;				" <a href=\"music.htm\">Music</a> |"
;;				" <a href=\"note.htm\">Note</a> |"
;;				" <a href=\"vid.htm\">Video</a> |"
;;				" <a href=\"media.htm\">Media</a> |"
				" <a href=\"_todo.htm\">Todo</a> |"
;; 				" <a href=\"_test.htm\">Test</a> |"
				" <a href=\"_index.htm\">Index</a> |"
;;				" <a href=\"_changes.htm\">Changes</a> |"
;;				" <a href=\"_about.htm\">About</a> |"
				"</div>\n"
				))

			  (insert "<div id=\"main\">")

;;images
              (mapcar
               (lambda (type)
                 (if (and (file-exists-p lens-img-dir)
					 (file-exists-p
;;(concat lens-output-dir "/" lens-img-dir "/" file "." type))
					  (concat lens-img-dir "/" file "." type)))
                     (insert (concat "<img src=\"" lens-img-dir "/" file "." type "\" alt=\"\"/>"))))

               '("jpg"
                 "gif"
                 "png"))

;;footer
              (goto-char (point-max))
              (insert
			   "</div>\n"
				"<div id=\"footer\">\n"
			   "  <a href=\".text/" file "\">Source</a> "
			   "last modified: " modified
			   "</div>\n"
			   "</body>\n"
			   "</html>\n")

              (save-buffer)
              (kill-buffer (current-buffer))
              )))
	  )))


;; generate placeholders for image files that don't yet have commentary
(defun lens-gen-missing-image-files ()
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
   (directory-files (concat lens-output-dir "/" lens-img-dir))))


;;lens-mode
(defvar lens-font-lock nil)

(defun lens-build-font-lock ()
  "fill `lens-font-lock' with all terms"
  (setq lens-font-lock nil)
  (lens-recurse
   (lambda (term)
     (setq lens-font-lock
           (concat
			(lens-shortest-inner term)
			"\\|" lens-font-lock)))) ;;fill with filenames

;;encase and terminate
  (setq lens-font-lock
        (concat "\\(\\)\\("
; 				lens-local-*nix-path "\\|"
				lens-quoted-local-M$-path "\\|"
				lens-local-M$-path "\\|"
				lens-quoted-UNC-path "\\|"
				lens-UNC-path "\\|"
				lens-explicit-URL "\\|"
				lens-implicit-HTTP "\\|"
				lens-font-lock "zzzzzzzz\\)")))
;; what a mess.


(defun lens-follow ()
  (save-excursion
    (re-search-forward lens-font-lock nil t))
  (let
	  ((match (match-string 2)))
	(if match
		(progn
		  (message match)
		  (if (file-exists-p match)
			  (find-file-read-only match)
			(if (file-exists-p (capitalize match))
				(find-file-read-only (capitalize match))
			  (if (file-exists-p (upcase match))
				  (find-file-read-only (upcase match))
				(find-file-read-only (downcase match)))))))))




(defun lens-mode-rebuild-mode ()
  (interactive)
  (lens-build-font-lock)
  (im-make-mode lens "lens --- Exposing connections." lens-font-lock lens-follow)
  (lens-mode))

;; KLUDGE: lens.el needs to be rewritten as a minor mode.
(lens-mode-rebuild-mode) ;;needed so we don't get error: (void-variable lens-mode-map)

(define-key lens-mode-map [(f5)] 'lens-mode-rebuild-mode)

(defvar lens-C-c-map (make-sparse-keymap))
(define-key lens-mode-map [(control c)] lens-C-c-map)

(define-key lens-C-c-map [(control c)]
  (lambda () (interactive) "Compile this file."
    (lens-make-page buffer-file-name)))

(define-key lens-C-c-map [(control p)]
  (lambda () (interactive)
	"Browse generated HTML."
    (browse-url-of-file
;;(message
     (file-truename
      (concat
	   lens-output-dir "/"
	   (file-relative-name
		(buffer-file-name)) ".htm")))))

(provide 'lens)

;;; lens.el ends here
