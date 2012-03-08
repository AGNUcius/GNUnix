;;; css-mode.el --- A minimal CSS mode.

;;; Commentary:
;; A (very) minimal CSS mode.  Does indentation, some font-locking and
;; nothing more.


;;; History:
;; Created <2002-08-29 22:09:39>

;;; Code:
(eval-when-compile
  (defvar comment-quote-nested))

(defvar css-mode-map nil
  "Keymap for `css-mode'.")

(defvar css-mode-indent-depth 4
  "*Depth of indentation.")

(defvar css-mode-font-lock-keywords-1
  (eval-when-compile
    (let ((keywords (regexp-opt
                     '("a" "abbr" "acronym" "address" "applet" "area" "b"
                       "base" "basefont" "bdo" "big" "blockquote" "body" "br"
                       "button" "caption" "center" "cite" "code" "col"
                       "colgroup" "dd" "del" "dfn" "dir" "div" "dl" "dt" "em"
                       "fieldset" "font" "form" "frame" "frameset" "h1" "h2"
                       "h3" "h4" "h5" "h6" "head" "hr" "html" "i" "iframe"
                       "img" "input" "ins" "isindex" "kbd" "label" "legend"
                       "li" "link" "map" "menu" "meta" "noframes" "noscript"
                       "object" "ol" "optgroup" "option" "p" "param" "pre" "q"
                       "s" "samp" "script" "select" "small" "span" "strike"
                       "strong" "style" "sub" "sup" "table" "tbody" "td"
                       "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "u"
                       "ul" "var"))))
      (list
       (list
        (concat "^\\(:?#[^ \t]+\\|\\.[^ \t]+\\|"
                keywords
                ;; css-mode-elements
                "\\)\\>\\(:?:[^ \t]+\\|.[ \t]*\\<\\(:?"
                keywords
                ;; css-mode-elements
                "\\)\\>\\)*")
        0 'font-lock-keyword-face))))
  "Normal level higlighting for `css-mode'.")

(defvar css-mode-font-lock-keywords-2
  (append css-mode-font-lock-keywords-1
          (eval-when-compile
            (let ((keywords (regexp-opt
                             '("azimuth" "background" "background-attachment" "background-color"
                               "background-image" "background-position" "background-repeat"
                               "border" "border-collapse" "border-color" "border-spacing"
                               "border-style" "border-top" "border-right" "border-bottom"
                               "border-left" "border-top-color" "border-right-color"
                               "border-bottom-color" "border-left-color" "border-top-style"
                               "border-right-style" "border-bottom-style" "border-left-style"
                               "border-top-width" "border-right-width" "border-bottom-width"
                               "border-left-width" "border-width" "bottom" "caption-side"
                               "clear" "clip" "color" "content" "counter-increment"
                               "counter-reset" "cue" "cue-after" "cue-before" "cursor"
                               "direction" "display" "elevation" "empty-cells" "float" "font"
                               "font-family" "font-size" "font-size-adjust" "font-stretch"
                               "font-style" "font-variant" "font-weight" "height" "left"
                               "letter-spacing" "line-height" "list-style" "list-style-image"
                               "list-style-position" "list-style-type" "margin" "margin-top"
                               "margin-right" "margin-bottom" "margin-left" "marker-offset"
                               "marks" "max-height" "max-width" "min-height" "min-width"
                               "orphans" "outline" "outline-color" "outline-style"
                               "outline-width" "overflow" "padding" "padding-top"
                               "padding-right" "padding-bottom" "padding-left" "page"
                               "page-break-after" "page-break-before" "page-break-inside"
                               "pause" "pause-after" "pause-before" "pitch" "pitch-range"
                               "play-during" "position" "quotes" "richness" "right" "size"
                               "speak" "speak-header" "speak-numeral" "speak-punctuation"
                               "speech-rate" "stress" "table-layout" "text-align"
                               "text-decoration" "text-indent" "text-shadow" "text-transform"
                               "top" "unicode-bidi" "vertical-align" "visibility" "voice-family"
                               "volume" "white-space" "widows" "width" "word-spacing"
                               "z-index"))))
              (list
               (list
                (concat "\\<\\("
                        keywords
                        "\\)\\>[ \t]*:")
                1 'font-lock-type-face)))))
  "Gaudy level highlighting for `css-mode'.")

;; this is too excessive.
;; (defvar css-mode-font-lock-keywords-3
;;   (append css-mode-font-lock-keywords-2
;;           (eval-when-compile
;;             (list
;;              (list
;;               ":[ \t]*\\(.*\\);"
;;               1 'font-lock-variable-name-face)
;;              (list
;;               ":[ \t]*\\(rgb\\)([ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*)"
;;               1 'font-lock-function-name-face 'prepend))))
;;   "Incredibly over-the-top highlighting for `css-mode'.")
              
(defvar css-mode-font-lock-keywords css-mode-font-lock-keywords-1
  "Default expressions to highlight in `css-mode'.")

(defvar css-mode-syntax-table nil
  "Syntax table for `css-mode'.")

(defun css-mode ()
  "Major mode for editing Cascading StyleSheets."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'css-mode
        mode-name "CSS")
  (use-local-map css-mode-map)
  ;; set up syntax table.
  (if css-mode-syntax-table
      ()
    (setq css-mode-syntax-table
          (let ((table (make-syntax-table)))
            ;; comment characters.
            (modify-syntax-entry ?/  ". 14" table)
            (modify-syntax-entry ?*  ". 23"   table)
            table)))
  (set-syntax-table css-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'comment-quote-nested)
  (setq comment-quote-nested t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-style)
  (setq comment-style 'extra-line)
  ;; (make-local-variable 'comment-indent-function)
  ;; (setq comment-indent-function 'css-mode-indent-comment)
  (setq indent-line-function 'css-mode-indent-line)
  (setq font-lock-defaults '((css-mode-font-lock-keywords
                              css-mode-font-lock-keywords-1
                              css-mode-font-lock-keywords-2))))

;; set up keymap
(if css-mode-map
    ()
  (setq css-mode-map (make-sparse-keymap))
  (define-key css-mode-map (kbd "RET") 'css-mode-newline-and-indent)
  (define-key css-mode-map [(tab)] 'css-mode-indent-line)
  (define-key css-mode-map [(})] 'css-mode-electric-insert-close-brace))


(defun css-mode-in-comment-p ()
  "Return t if `point' is in a comment.

Only works if buffer is fontified."
  (memq 'font-lock-comment-face (text-properties-at (point))))

(defun css-mode-line-empty-p ()
  "Return t if the current line is empty."
  (save-excursion
    (forward-line 0)
    (looking-at "^[ \t]*$")))

(defun css-mode-opening-line-of-block-p ()
  "Return t if the last character on the current line is {."
  (save-excursion
    (end-of-line)
    (css-mode-skip-back-over-comments (point-at-bol))
    (= (preceding-char) ?\{)))

(defun css-mode-closing-line-of-block-p ()
  "Return t if the last character on the current line is }."
  (save-excursion
    (end-of-line)
    (css-mode-skip-back-over-comments (point-at-bol))
    (= (preceding-char) ?\})))

(defun css-mode-line-closed-p ()
  "Return t if the last character on the current line is ;."
  (save-excursion
    (end-of-line)
    (css-mode-skip-back-over-comments (point-at-bol))
    (= (preceding-char) ?\;)))

(defun css-mode-skip-back-over-comments (&optional bound)
  "Skip backwards from `point' over comments.

The function skips backward over one comment [using
\(forward-comment -1)], and then skips backwards over whitespace.

If BOUND is non-nil, don't go back further than it."
  (save-restriction
    (if bound
        (narrow-to-region (point) bound))
    (forward-comment -1)
    (skip-chars-backward " \t")))

(defun css-mode-find-context ()
  "Find the context for the current line.

Returns a plist of context values.
A typical plist might be:
\(line-closed nil prev-line-closed t line-closes-block nil
 line-opens-block nil in-block t line-empty t).

This context set would be for a line in the following example context:

html {
    font-size : 100%;
    <- point on this line ->
}

The context types are (all descriptions presume comments have been ignored.):

   line-closed       --- the current line ends in a \";\" character.
   prev-line-closed  --- the previous line ends in a \";\" character.
   line-closes-block --- the current line ends in a \"}\" character.
   line-opens-block  --- the current-line ends in a \"{\" character.
   in-block          --- `point' is in a \"{ }\" block, or after a
                        \"{\" character with no matching \"}\" character. 
   line-empty        --- the current line consists only of whitespace,
                         i.e. it matches the regexp \"^[ \\t]*$\"."
  (let ((line-closed (css-mode-line-closed-p))
        (prev-line-closed (save-excursion (forward-line -1)
                                          (while (css-mode-line-empty-p)
                                            (forward-line -1))
                                          (css-mode-line-closed-p)))
        (line-closes-block (css-mode-closing-line-of-block-p))
        (line-opens-block (css-mode-opening-line-of-block-p))
        (in-block (css-mode-in-block-p))
        (line-empty (css-mode-line-empty-p)))
    (list 'line-closed line-closed
          'prev-line-closed prev-line-closed
          'line-closes-block (or line-closes-block)
          'line-opens-block (or line-opens-block )
          'in-block in-block
          'line-empty line-empty)))

(defmacro css-mode-get-context (context-type context)
  "Return t if CONTEXT-TYPE is part of the current CONTEXT.

CONTEXT is found through the function `css-mode-find-context'."
  `(cadr (memq ,context-type ,context)))

(defun css-mode-calc-indent-level ()
  "Calculate the indent level for the current line."
  (save-excursion
    (let* ((indent css-mode-indent-depth)
           (context (css-mode-find-context))
           (open-block (css-mode-get-context 'line-opens-block context))
           (closed-block (css-mode-get-context 'line-closes-block context))
           (closed-line (css-mode-get-context 'line-closed context))
           (prev-line-closed (css-mode-get-context 'prev-line-closed context))
           (open-line (not closed-line))
           (in-block (css-mode-get-context 'in-block context))
           (line-empty (css-mode-get-context 'line-empty context)))
      (cond ((css-mode-in-comment-p)
             (css-mode-indent-comment))
            ((or (not in-block)
                 open-block
                 closed-block)
             0)
            (line-empty
             (if in-block
                 indent
               0))
            ((not prev-line-closed)
             (+ indent (save-excursion (forward-line -1)
                                       (back-to-indentation)
                                       (current-column))))
            (closed-line
             indent)
            (open-line
             (+ indent (save-excursion (back-to-indentation) (current-column))))
            (t
             indent)))))

(defun css-mode-in-block-p ()
  "Non-nil if point is in a {} block."
  (save-excursion
    (let ((point (point))
          (open-paren (or (search-backward "{" nil t) most-positive-fixnum))
          (close-paren-below (or (search-forward "}" nil t) most-positive-fixnum)))
      (and (< open-paren point)
           (< point close-paren-below)))))

(defun css-mode-indent-line (&optional indent)
  "Indent the current line.

If optional INDENT is non-nil, use that instead of calculating the
indent level."
  (interactive)
  (back-to-indentation)
  (delete-region (point-at-bol) (point))
  (indent-to (or indent (css-mode-calc-indent-level))))

(defun css-mode-indent-comment ()
  "Calculate the indent level for this comment."
  (let ((indent (back-to-indentation)))
    (save-excursion
      (forward-line 0)
      (unless (looking-at "/\\*")
        (forward-line -1)
        (end-of-line)
        (search-backward "*" (point-at-bol) t)
        (setq indent (current-column))))
    indent))
        
(defun css-mode-newline-and-indent ()
  "Insert a newline and indent."
  (interactive)
  (newline)
  (css-mode-indent-line))

(defun css-mode-electric-insert-close-brace ()
  "Insert a closing brace }."
  (interactive)
  (insert "}")
  (css-mode-indent-line)
  (forward-char))

(provide 'css-mode)

;;; css-mode.el ends here
