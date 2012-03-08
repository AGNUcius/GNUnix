;;;; based on http://www.emacswiki.org/elisp/menu-bar-plus.el

(defvar menu-bar-divider-menu (make-sparse-keymap "Divider"))
(define-key global-map [menu-bar divider] (cons "||" menu-bar-divider-menu))
(define-key menu-bar-divider-menu [menu-bar-divider-hint]
  '("<-- Current mode menus to left.   ||   Common menus to right -->"
    . describe-menubar))

;;;###autoload
(defun describe-menubar ()
  "Explain the menu bar, in general terms."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (substitute-command-keys
            "To the right of the menu bar divider (\"||\") are the general menus
that always appear in every buffer.  To the left of this symbol, there
may also be additional menus that are specific to the buffer's mode
\(use `\\[describe-mode]' for information on a buffer's mode).

The general menus are as follows:

    Buffers  Files  Tools  Edit  Search Mule  Frames  Help

Use the \"Frames\" menu to resize and hide/show frames.  The \"Help\" menu is an
extension of the \"Help\" menu described in the Emacs manual (`\\[info]').

For information on a menu item, use the \"This\" item in the \"Describe\"
submenu of the \"Help\" menu."))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      (help-mode)
      (buffer-string))))                ; Return the text we displayed.

;;;###autoload
(defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
(define-key global-map [menu-bar frames] (cons "Frames" menu-bar-frames-menu))

;; REPLACES ORIGINAL defined in `menu-bar.el'.
(defvar menu-bar-files-menu (make-sparse-keymap "Files"))
(define-key global-map [menu-bar files] (cons "Files" menu-bar-files-menu))

;; REPLACES ORIGINAL defined in `menu-bar.el'.
(defvar menu-bar-edit-menu (make-sparse-keymap "Edit"))
(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))
;; Remove default bindings.

;; REPLACES ORIGINAL defined in `menu-bar.el'.
(global-unset-key [menu-bar search])
(defconst menu-bar-search-menu (make-sparse-keymap "Search"))
(define-key global-map [menu-bar search] (cons "Search" menu-bar-search-menu))

;;;@@@Emacs20 (defvar menu-bar-help-menu (make-sparse-keymap "Help")) ; Wipe out original.
;;;@@@Emacs20 (define-key global-map [menu-bar help-menu] (cons "?" menu-bar-help-menu))

;; Change main menu-bar order.
(setq menu-bar-final-items '(divider buffer files tools edit search mule frames help-menu))


;;; FRAMES menu.
(define-key menu-bar-frames-menu [iconify-everything]
  '("Iconify All Frames" . iconify-everything))
(define-key menu-bar-frames-menu [show-hide]
  '("Hide Frames / Show Buffers" . show-hide))
(define-key menu-bar-frames-menu [shrink-frame-to-fit]
  '("Shrink-Wrap This Frame" . shrink-frame-to-fit)) ; Defined in `shrink-fit.el'.


;;; FILES menu.
;; Remove some default bindings.
(global-unset-key [menu-bar files separator-buffers])
(global-unset-key [menu-bar files separator-exit])

(define-key menu-bar-files-menu [open-file]
  (if (and (fboundp 'dlgopen-open-files) (string-match "i386" system-configuration))
      '("Open File..." . dlgopen-open-files)
    '("Open File..." . find-file-other-frame)))
(define-key-after menu-bar-files-menu [dired]
  '("Open Directory..." . dired-other-frame) 'open-file)
(define-key-after menu-bar-files-menu [save-buffer] '("Save" . save-buffer) 'dired)
(define-key-after menu-bar-files-menu [write-file]
  '("Save As..." . write-file) 'save-buffer)
(define-key-after menu-bar-files-menu [revert-buffer]
  '("Revert" . revert-buffer) 'write-file)
(define-key-after menu-bar-files-menu [insert-file]
  '("Insert File..." . insert-file) 'revert-buffer)
;;--------------------
(define-key-after menu-bar-files-menu [separator-frames] '("--") 'insert-file)
(define-key-after menu-bar-files-menu [one-window]
  '("One Window" . delete-other-windows) 'separator-frames)
(define-key-after menu-bar-files-menu [split-window]
  '("Split Window" . split-window-vertically) 'one-window)
(when (fboundp 'delete-frame) ; Don't use delete-frame as event name: it's a special event.
  (define-key-after menu-bar-files-menu [delete-this-frame]
    '("Delete Frame" . delete-frame) 'split-window)
  (define-key-after menu-bar-files-menu [make-frame-on-display]
    '("Open New Display..." . make-frame-on-display) 'delete-this-frame)
  (define-key-after menu-bar-files-menu [make-frame]
    '("Make New Frame" . make-frame) 'make-frame-on-display))
;;--------------------
(define-key-after menu-bar-files-menu [separator-exit] '("--")
  'make-frame)
(define-key-after menu-bar-files-menu [exec-cmd]
  '("Execute Command" . execute-extended-command) 'separator-exit)
(define-key-after menu-bar-files-menu [repeat-cmd]
  '("Repeat Earlier Command" . repeat-complex-command) 'exec-cmd)
(define-key-after menu-bar-files-menu [separator-exit2] '("--")
  'repeat-cmd)
(define-key menu-bar-files-menu [kill-buffer]
  '("Kill This Buffer" . kill-this-buffer))
;; Done in `menu-bar.el':
;; (put 'kill-this-buffer 'menu-enable '(kill-this-buffer-enabled-p))
(define-key menu-bar-files-menu [exit-emacs]
  '("Exit Emacs..." . exit-with-confirmation)) ; Defined in `misc-cmds.el'. (defsubst)


;; REPLACES ORIGINAL in `menu-bar.el':
;; Deletes buffer's windows as well.  defun -> defsubst.
(defun kill-this-buffer ()
"Delete the current buffer and delete all of its windows."
  (interactive)
  (if (and (boundp 'sub-kill-buffer-and-its-windows) ; In setup-keys.el.
           sub-kill-buffer-and-its-windows
           (fboundp 'kill-buffer-and-its-windows))
      (kill-buffer-and-its-windows (current-buffer)) ;`misc-cmds.el'
    (kill-buffer (current-buffer))))    ; <-- original defn.


;;; TOOLS menu.
;;;@@@Emacs20 ;; Remove some default bindings.
;;;@@@Emacs20 (global-unset-key [menu-bar tools separator-print])
;;;@@@Emacs20 (global-unset-key [menu-bar tools ps-print-region])
;;;@@@Emacs20 (global-unset-key [menu-bar tools ps-print-buffer])
;;;@@@Emacs20 (global-unset-key [menu-bar tools print-region])
;;;@@@Emacs20 (global-unset-key [menu-bar tools print-buffer])
;;;@@@Emacs20 
;;;@@@Emacs20 (defvar menu-bar-print-menu (make-sparse-keymap "Print"))
;;;@@@Emacs20 (define-key menu-bar-tools-menu [print] (cons "Print" menu-bar-print-menu))
;;;@@@Emacs20 (define-key-after menu-bar-tools-menu [separator-print] '("--") 'print)
;;;@@@Emacs20 (define-key-after menu-bar-tools-menu [separator-cal] '("--") 'rmail)
;;;@@@Emacs20 (define-key-after menu-bar-tools-menu [show-calendar] '("Show Calendar" . show-calendar) 'separator-cal)
;;;@@@Emacs20 (define-key-after menu-bar-tools-menu [calendar] '("Calendar and Reminders" . calendar) 'calendar)
 
;;;@@@Emacs20 ;; EDIFF submenu of TOOLS
(when (fboundp 'vc-ediff)
  (define-key menu-bar-ediff-menu [ediff-revision] ; Defined in `vc+.el'.
    '("File with Revision..." . vc-ediff))
  (define-key-after menu-bar-ediff-menu [vc-diff] ; Defined in `vc+.el'.
    '("File with Revision using Diff" . vc-diff) 'ediff-revision)
  (define-key-after menu-bar-ediff-menu [diff]
    '("Two Files using Diff..." . diff) 'ediff-files)) ; `diff+.el'

;;;@@@Emacs20 ;; PRINT submenu of TOOLS
;;;@@@Emacs20 ;; The `*-declp-*' commands are defined in `misc-cmds.el'.
;;;@@@Emacs20 (define-key menu-bar-print-menu [ps-print-region]
;;;@@@Emacs20   '("Region as Postscript" . ps-print-region-with-faces))
;;;@@@Emacs20 ;(put 'declp-region-w-switches 'menu-enable 'mark-active)
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [declp-region-w-switches]
;;;@@@Emacs20 ;  '("Region with Switches..." . declp-region-w-switches))
;;;@@@Emacs20 ;(put 'pr-declp-region 'menu-enable 'mark-active)
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [print-paged-region]
;;;@@@Emacs20 ;  '("Paged Region..." . pr-declp-region))
;;;@@@Emacs20 ;(put 'declp-region 'menu-enable 'mark-active)
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [print-region]
;;;@@@Emacs20 ;  '("Region..." . declp-region))
;;;@@@Emacs20 ;;--------------------
;;;@@@Emacs20 (define-key menu-bar-print-menu [separator-print-buffer] '("--"))
;;;@@@Emacs20 (define-key menu-bar-print-menu [ps-print-buffer]
;;;@@@Emacs20   '("Buffer as Postscript" . ps-print-buffer-with-faces))
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [declp-buffer-w-switches]
;;;@@@Emacs20 ;  '("Buffer with Switches..." . declp-buffer-w-switches))
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [print-paged-buffer]
;;;@@@Emacs20 ;  '("Paged Buffer..." . pr-declp-buffer))
;;;@@@Emacs20 ;(define-key menu-bar-print-menu [print-buffer]
;;;@@@Emacs20 ;  '("Buffer..." . declp-buffer))

;;;@@@Emacs20 ;;; VERSION CONTROL submenu of TOOLS
;;;@@@Emacs20 (when (boundp 'vc-menu-map)
;;;@@@Emacs20 ;; Remove some default bindings.
;;;@@@Emacs20 (global-unset-key [menu-bar tools vc separator1])
;;;@@@Emacs20 (global-unset-key [menu-bar tools vc separator2])
;;;@@@Emacs20 (define-key vc-menu-map [vc-status-here] '("Files Here" . vc-status-here)))
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-directory] '("Files Below" . vc-directory) 'vc-status-here)
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-dir-separator1] '("--") 'vc-directory)
;;;@@@Emacs20 (define-key-after vc-menu-map [ediff-revision] '("Compare with Version..." . vc-ediff) 'vc-dir-separator1)
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-diff] '("Compare Last Version using Diff" . vc-diff) 'ediff-revision)
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-version-other-window]
;;;@@@Emacs20   '("Show Other Version..." . vc-version-other-window) 'vc-diff)
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-dir-separator2] '("--") 'vc-version-other-window)
;;;@@@Emacs20 (define-key-after vc-menu-map [vc-rename-file] '("Rename File..." . vc-rename-file)
;;;@@@Emacs20   'vc-register)
;;;@@@Emacs20 (define-key vc-menu-map [vc-check-out] '("Check In/Out" . vc-toggle-read-only))
;;;@@@Emacs20 
;;;@@@Emacs20 (put 'vc-diff 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-ediff 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-version-other-window 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-toggle-read-only 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs20 (put 'vc-insert-headers 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs20 (put 'vc-register 'menu-enable '(not vc-mode)) ; vc-dired-mode is OK (e.g. Unregistered).
;;;@@@Emacs20 (put 'vc-rename-file 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-revert-buffer 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-cancel-version 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-print-log 'menu-enable '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs20 (put 'vc-update-change-log 'menu-enable (or (eq (vc-buffer-backend) 'RCS) vc-dired-mode (eq 'dired-mode major-mode)))


;;; EDIT menu
;; Remove some default bindings.
;;;@@@Emacs20 (global-unset-key [menu-bar edit separator-edit])

;;;@@@Emacs20 (define-key menu-bar-edit-menu [undo] '("Undo" . advertised-undo))
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [separator-edit-undo] '("--") 'undo)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [cut] '("Cut" . kill-region) 'separator-edit-undo)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [copy] '("Copy" . menu-bar-kill-ring-save) 'cut)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [paste] '("Paste" . yank) 'copy)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [select-paste] '("Select and Paste" . yank-menu)
;;;@@@Emacs20   'paste)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [clear] '("Clear" . delete-region) 'select-paste)
(define-key-after menu-bar-edit-menu [yank-secondary]
  '("Paste Secondary" . yank-secondary) 'clear) ; In `misc-cmds.el'
(put 'yank-secondary 'menu-enable '(x-selection-exists-p 'SECONDARY))
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-cut] '("--")
  'yank-secondary)
(define-key-after menu-bar-edit-menu [select-all]
  '("Select All" . mark-whole-buffer) 'separator-edit-cut)
(define-key-after menu-bar-edit-menu [flush-lines] ; Defined in `replace+.el'.
  '("Delete Matching Lines..." . flush-lines) 'select-all)
(define-key-after menu-bar-edit-menu [keep-lines] ; Defined in `replace+.el'.
  '("Delete Non-Matching Lines..." . keep-lines) 'flush-lines)
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-select-all] '("--")
  'keep-lines)
(defvar menu-bar-edit-fill-menu (make-sparse-keymap "Fill"))
;;;@@@Emacs20 (defalias 'menu-bar-edit-fill-menu (symbol-value 'menu-bar-edit-fill-menu))
(define-key-after menu-bar-edit-menu [props] '("Text Properties" . facemenu-menu) 'separator-edit-select-all)
(define-key-after menu-bar-edit-menu [fill]
  (cons "Fill" menu-bar-edit-fill-menu) 'props)
(defvar menu-bar-edit-region-menu (make-sparse-keymap "Edit Region"))
(defalias 'menu-bar-edit-region-menu (symbol-value 'menu-bar-edit-region-menu))
(define-key-after menu-bar-edit-menu [region]
  (cons "Edit Region" menu-bar-edit-region-menu) 'fill)
(defvar menu-bar-edit-sort-menu (make-sparse-keymap "Sort Region"))
(defalias 'menu-bar-edit-sort-menu (symbol-value 'menu-bar-edit-sort-menu))
(define-key-after menu-bar-edit-menu [sort]
  (cons "Sort Region" menu-bar-edit-sort-menu) 'region)
;;;@@@Emacs20 (when (fboundp 'start-process)
;;;@@@Emacs20 (define-key-after menu-bar-edit-menu [spell] '("Spell" . ispell-menu-map) 'sort))

;; EDIT FILL submenu.
(define-key menu-bar-edit-fill-menu [fill-nonuniform-para]
  '("Fill Non-Uniform �'s" . fill-nonuniform-paragraphs))
(put 'fill-nonuniform-paragraphs 'menu-enable 'mark-active)
(define-key menu-bar-edit-fill-menu [fill-indiv-para]
  '("Fill Uniform �'s" . fill-individual-paragraphs))
(put 'fill-individual-paragraphs 'menu-enable 'mark-active)
(define-key menu-bar-edit-fill-menu [fill-region] '("Fill �'s" . fill-region))
(define-key menu-bar-edit-fill-menu [fill-para]
  '("Fill �" . fill-paragraph-ala-mode))

(defun fill-paragraph-ala-mode (&optional arg)
  "Do whatever `M-q' does, if it is bound.  Else, `fill-paragraph'.
Normally, this fills a paragraph according to the current major mode.
For example, in C Mode, `M-q' is normally bound to `c-fill-paragraph',
and in Lisp Mode, `M-q' is normally bound to `lisp-fill-paragraph'."
  (let ((map (current-local-map)))
    (or (and map (funcall (lookup-key map "\M-q") arg))
        (funcall (lookup-key (current-global-map) "\M-q") arg)
        (fill-paragraph arg))))

;; EDIT REGION submenu.
(define-key menu-bar-edit-region-menu [unaccent-region]
  '("Unaccent" . unaccent-region))      ; Defined in `unaccent'.
(put 'unaccent-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [capitalize-region]
  '("Capitalize" . capitalize-region))
(put 'capitalize-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [downcase-region]
  '("Downcase" . downcase-region))
(put 'downcase-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [upcase-region]
  '("Upcase" . upcase-region))
(put 'upcase-region 'menu-enable 'mark-active)
;;--------------------
(define-key menu-bar-edit-region-menu [separator-chars] '("--"))
(define-key menu-bar-edit-region-menu [unhighlight-region]
  '("Unhighlight". unhighlight-region))
(put 'unhighlight-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [highlight-regexp-region]
  '("Highlight Regexp..." . highlight-regexp-region))
(put 'highlight-regexp-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [highlight-region]
  '("Highlight..." . highlight-region))
(put 'highlight-region 'menu-enable 'mark-active)
;;--------------------
(define-key menu-bar-edit-region-menu [separator-highlight] '("--"))
(define-key menu-bar-edit-region-menu [untabify-region]
  '("Untabify" . untabify-region))
(put 'untabify-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [tabify-region]
  '("Tabify" . tabify-region))
(put 'tabify-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [comment-region]
  '("(Un)Comment" . comment-region))
(put 'comment-region 'menu-enable '(and mark-active comment-start))
(define-key menu-bar-edit-region-menu [center-region]
  '("Center" . center-region))
(put 'center-region 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [indent-rigidly-region]
  '("Rigid Indent" . indent-rigidly))
(put 'indent-rigidly 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [indent-region]
  '("Column/Mode Indent" . indent-region))
(put 'indent-region 'menu-enable 'mark-active)
;;--------------------
(define-key menu-bar-edit-region-menu [separator-indent] '("--"))
(define-key menu-bar-edit-region-menu [abbrevs-region]
  '("Expand Abbrevs" . expand-region-abbrevs))
(put 'expand-region-abbrevs 'menu-enable 'mark-active)
(define-key menu-bar-edit-region-menu [macro-region]
  '("Exec Keyboard Macro" . apply-macro-to-region-lines)) ; In `macros+.el'.
(put 'apply-macro-to-region-lines 'menu-enable
     '(and last-kbd-macro mark-active))

;; EDIT SORT submenu.
(define-key menu-bar-edit-sort-menu [sort-regexp-fields]
  '("Regexp Fields..." . sort-regexp-fields))
(put 'sort-regexp-fields 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-pages]
  '("Pages" . sort-pages))
(put 'sort-pages 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-paragraphs]
  '("Paragraphs" . sort-paragraphs))
(put 'sort-paragraphs 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-numeric-fields]
  '("Numeric Field" . sort-numeric-fields))
(put 'sort-numeric-fields 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-fields]
  '("Field" . sort-fields))
(put 'sort-fields 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-columns]
  '("Columns" . sort-columns))
(put 'sort-columns 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [sort-lines]
  '("Lines" . sort-lines))
(put 'sort-lines 'menu-enable 'mark-active)
(define-key menu-bar-edit-sort-menu [reverse-region]
  '("Reverse" . reverse-region))
(put 'reverse-region 'menu-enable 'mark-active)


;;; SEARCH menu.

(defun nonincremental-repeat-word-search-forward ()
  "Search forward for the previous search string."

  (interactive)
  (word-search-forward (car search-ring)))

(defun nonincremental-repeat-word-search-backward ()
  "Search backward for the previous search string."
  (interactive)
  (word-search-backward (car search-ring)))

(define-key menu-bar-search-menu [reminder6] '(" " . %$>disabled@!^))
(define-key menu-bar-search-menu [reminder5]
  (cons (substitute-command-keys
         "  Incr. Regexp Search: \\[isearch-forward-regexp], \
\\[isearch-backward-regexp]") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder4]
  (cons (substitute-command-keys
         " Word Search: \\[isearch-forward] RET C-w, \\[isearch-backward] \
RET C-w") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder3]
  (cons (substitute-command-keys
         "Incr. Search: \\[isearch-forward], \\[isearch-backward]  \
(\\[isearch-forward] C-h: Help)") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder2]
  '("           ** Reminder **" . %$>disabled@!^))
(define-key menu-bar-search-menu [reminder1] '(" " . %$>disabled@!^))
(put '%$>disabled@!^ 'menu-enable '(not t))
;;--------------------
(define-key menu-bar-search-menu [separator-search-multiple] '("--"))
(define-key menu-bar-search-menu [bookmark]
  '("Bookmarks" . menu-bar-bookmark-map))
(defvar menu-bar-search-tags-menu (make-sparse-keymap "Tags"))
(defalias 'menu-bar-search-tags-menu
  (symbol-value 'menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [tags]
  (cons "Tags" menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [occur] '("Occurrences..." . occur))
(define-key menu-bar-search-menu [grep] '("Grep..." . grep)) ; In `compile.el'. 
;;--------------------
(define-key menu-bar-search-menu [separator-search-replace] '("--"))
(defvar menu-bar-search-replace-menu (make-sparse-keymap "Replace"))
(defalias 'menu-bar-search-replace-menu (symbol-value 'menu-bar-search-replace-menu))
(define-key menu-bar-search-menu [replace]
  (cons "Replace" menu-bar-search-replace-menu))
;;--------------------
(define-key menu-bar-search-menu [separator-search-word] '("--"))
(define-key menu-bar-search-menu [repeat-word-search-back]
  '("             Again" . nonincremental-repeat-word-search-backward))
(define-key menu-bar-search-menu [word-search-back]
  '("     Backward..." . word-search-backward))
(define-key menu-bar-search-menu [repeat-word-search-fwd]
  '("             Again" . nonincremental-repeat-word-search-forward))
(define-key menu-bar-search-menu [word-search-fwd]
  '("Word Forward..." . word-search-forward))
;;--------------------
(define-key menu-bar-search-menu [separator-search-re] '("--"))
(define-key menu-bar-search-menu [repeat-regexp-back]
  '("               Again" . nonincremental-repeat-re-search-backward))
(define-key menu-bar-search-menu [re-search-backward]
  '("       Backward..." . nonincremental-re-search-backward))
(define-key menu-bar-search-menu [repeat-regexp-fwd]
  '("               Again" . nonincremental-repeat-re-search-forward))
(define-key menu-bar-search-menu [re-search-forward]
  '("Regexp Forward..." . nonincremental-re-search-forward))
;;--------------------
(define-key menu-bar-search-menu [separator-search] '("--"))
(define-key menu-bar-search-menu [repeat-search-back]
  '("               Again" . nonincremental-repeat-search-backward))
(define-key menu-bar-search-menu [search-backward]
  '("String Backward..." . nonincremental-search-backward))
(define-key menu-bar-search-menu [repeat-search-fwd]
  '("               Again" . nonincremental-repeat-search-forward))
(define-key menu-bar-search-menu [search-forward]
  '("String Forward..." . nonincremental-search-forward))

;;; SEARCH TAGS submenu.
(define-key menu-bar-search-tags-menu [tags-search]
  (cons (substitute-command-keys
         "Search Tags Files...  (again: \\[tags-loop-continue])")
        'tags-search))
(define-key menu-bar-search-tags-menu [find-tag-regexp]
  '("Find Tag Regexp..." . find-tag-regexp))
(define-key menu-bar-search-tags-menu [find-tag-other-frame]
  '("Find Tag..." . find-tag-other-frame))

;; REPLACE submenu
(define-key menu-bar-search-replace-menu [replace-regexp]
  '("       Regexp..." . replace-regexp))
(define-key menu-bar-search-replace-menu [replace-string]
  '("Global String..." . replace-string))
;;--------------------
(define-key menu-bar-search-replace-menu [separator-search-replace-global] '("--"))
(define-key menu-bar-search-replace-menu [tags-query-replace]
  (cons (substitute-command-keys
         "            Tags... (again: \\[tags-loop-continue])")
        'tags-query-replace))
(define-key menu-bar-search-replace-menu [map-query-replace-regexp]
  '("            Map..." . map-query-replace-regexp))
(define-key menu-bar-search-replace-menu [query-replace-regexp]
  '("      Regexp..." . query-replace-regexp))
(if (fboundp 'query-replace-w-options)  ; Defined in `replace+.el'.
    (define-key menu-bar-search-replace-menu [query-replace-w-options]
      '("Query String" . query-replace-w-options))
  (define-key menu-bar-search-replace-menu [query-replace-w-options]
    '("Query String" . query-replace)))
(put 'replace-regexp 'menu-enable '(not buffer-read-only))
(put 'replace-string 'menu-enable '(not buffer-read-only))
(put 'tags-query-replace 'menu-enable '(not buffer-read-only))
(put 'map-query-replace-regexp 'menu-enable '(not buffer-read-only))
(put 'query-replace-w-options 'menu-enable '(not buffer-read-only))
;; Done in menu-bar.el:
;; (put 'query-replace 'menu-enable '(not buffer-read-only))
;; (put 'query-replace-regexp 'menu-enable '(not buffer-read-only))


;;; HELP menu.

;;; General help
(define-key menu-bar-help-menu [separator-genl-help] '("--"))
(define-key menu-bar-help-menu [save-*Help*-buffer]
  '("Save *Help* Buffer" . save-*Help*-buffer)) ; In `help+.el'.
(define-key menu-bar-help-menu [show-*Help*-buffer]
  '("Show *Help* Buffer" . show-*Help*-buffer)) ; In `frame-cmds.el' (defsubst)
(define-key menu-bar-help-menu [help-for-help]
  '("Help on Help..." . help-for-help)) ;

;;; Remove some default bindings
(define-key menu-bar-help-menu [finder-by-keyword] nil)
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(define-key menu-bar-help-menu [sep2] nil)

;;; Whoops!? submenu
(defvar menu-bar-whereami-menu (make-sparse-keymap "Whoops!?"))
(define-key menu-bar-help-menu [whereami]
  (cons "Whoops!?" menu-bar-whereami-menu))
(define-key menu-bar-whereami-menu [view-lossage]
  '("What did I do !?" . view-lossage)) ; Defined in `help.el'.
(define-key menu-bar-whereami-menu [top-level]
  '("Back to Top Level" . top-level))
(define-key menu-bar-whereami-menu [keyboard-quit]
  '("Cancel Current Action" . keyboard-quit))

;;; Apropos submenu
(defvar menu-bar-apropos-menu (make-sparse-keymap "Apropos"))
(define-key-after menu-bar-help-menu [apropos]
  (cons "Apropos" menu-bar-apropos-menu) 'separator-genl-help)
(define-key menu-bar-apropos-menu [apropos-doc] ; Defined in `apropos.el'.
  '("Symbol Descriptions..." . apropos-documentation))
(define-key menu-bar-apropos-menu [apropos-tags]
  '("Tags..." . tags-apropos))
(define-key menu-bar-apropos-menu [apropos-symbols] ; Defined in `apropos.el'
  '("Symbols..." . apropos))
(define-key menu-bar-apropos-menu [apropos-variables]
  '("All Variables..." . apropos-variable))
(when (fboundp 'apropos-user-options)
  (define-key menu-bar-apropos-menu [apropos-user-options]
    '("User Options..." . apropos-user-options)))
(define-key menu-bar-apropos-menu [apropos-command]
  '("Commands..." . apropos-command))

;;; Describe submenu
;;; Remove predefined version first.
(defconst menu-bar-describe-menu (make-sparse-keymap "Describe"))
(define-key-after menu-bar-help-menu [describe]
  (cons "Describe" menu-bar-describe-menu) 'apropos)
(when (fboundp 'help-on-click/key)
  (define-key menu-bar-describe-menu [help-on-click]
    '("This..." . help-on-click/key)))  ; Defined in `help+.el'.
(define-key-after menu-bar-describe-menu [describe-mode]
  '("Buffer Modes" . describe-mode) 'help-on-click)
(define-key-after menu-bar-describe-menu [describe-key]
  '("Key..." . describe-key) 'describe-mode)
(define-key-after menu-bar-describe-menu [describe-function]
  '("Function..." . describe-function) 'describe-key)
(define-key-after menu-bar-describe-menu [describe-variable]
  '("Variable..." . describe-variable) 'describe-function)
(define-key-after menu-bar-describe-menu [list-keybindings]
  '("All Key Bindings" . describe-bindings) 'describe-variable)
(when (fboundp 'describe-menubar)
  (define-key-after menu-bar-describe-menu [describe-menubar]
    '("Menu Bar" . describe-menubar) 'list-keybindings))
(define-key-after menu-bar-describe-menu [describe-syntax]
  '("Major Mode Syntax" . describe-syntax) 'describe-menubar)

;;; Manuals submenu.

;; REPLACES ORIGINAL defined in `menu-bar.el'.
;; Remove some default bindings.
;; Name changes.
(defconst menu-bar-manuals-menu (make-sparse-keymap "Learn More"))
(define-key-after menu-bar-help-menu [manuals]
  (cons "Learn More" menu-bar-manuals-menu) 'describe)
(define-key-after menu-bar-help-menu [separator-manuals] '("--") 'manuals)
(define-key menu-bar-manuals-menu [man]
  '("Unix Man Page..." . manual-entry))
(when (fboundp 'dir-info)
  (define-key menu-bar-manuals-menu [dir-info]
    '("All `Info' Manuals" . dir-info)))
(define-key menu-bar-manuals-menu [info]
  '("Last Accessed `Info' Manual" . info))
(define-key menu-bar-manuals-menu [emacs-faq] nil)
(define-key menu-bar-manuals-menu [emacs-news] nil)
(define-key menu-bar-manuals-menu [key] nil)
(define-key menu-bar-manuals-menu [command] nil)


;;; Emacs Lisp submenu of Manuals submenu.
(defvar menu-bar-emacs-lisp-manual-menu (make-sparse-keymap "Emacs Lisp"))
(define-key menu-bar-manuals-menu [emacs-lisp-manual]
  (cons "Emacs Lisp" menu-bar-emacs-lisp-manual-menu))
(define-key menu-bar-emacs-lisp-manual-menu [emacs-Lisp-News]
  '("Change History" . view-emacs-lisp-news)) ; Defined in `help.el'.
(define-key menu-bar-emacs-lisp-manual-menu [finder-by-keyword]
  '("Locate Libraries by Keyword" . finder-by-keyword)) ; Defined in `finder.el'.
(define-key menu-bar-emacs-lisp-manual-menu [locate-library] ; Defined in `help.el'.
  '("Locate Library..." . locate-library))
(define-key menu-bar-emacs-lisp-manual-menu [emacs-lisp-manual-separator] '("--"))
(when (fboundp 'emacs-lisp-info)
  (define-key menu-bar-emacs-lisp-manual-menu [emacs-lisp-info]
    '("Manual (`Info')" . emacs-lisp-info)))

;;; Emacs submenu of Manuals submenu.
(defvar menu-bar-emacs-manual-menu (make-sparse-keymap "Emacs"))
(define-key menu-bar-manuals-menu [emacs-manual]
  (cons "Emacs" menu-bar-emacs-manual-menu))
(define-key menu-bar-emacs-manual-menu [emacs-faq]
  '("FAQ" . view-emacs-FAQ))
(define-key menu-bar-emacs-manual-menu [emacs-news]
  '("Change History" . view-emacs-news))
(define-key menu-bar-emacs-manual-menu [emacs-manual-separator] '("--"))
(define-key menu-bar-emacs-manual-menu [key]
  '("Find Key in Manual" . Info-goto-emacs-key-command-node))
(define-key menu-bar-emacs-manual-menu [command]
  '("Find Command in Manual" . Info-goto-emacs-command-node))
(when (fboundp 'emacs-info)
  (define-key menu-bar-emacs-manual-menu [emacs-info]
    '("Manual (`Info')" . emacs-info)))
(define-key menu-bar-emacs-manual-menu [emacs-tutorial]
  '("Tutorial" . help-with-tutorial))

;;; Options submenu.
(define-key menu-bar-options-menu [all-options-separator] '("--"))
(define-key menu-bar-options-menu [edit-options]
  '("All Options" . edit-options))
(when (featurep 'icomplete)
  (define-key-after menu-bar-options-menu [icomplete-mode]
    (menu-bar-make-toggle toggle-icomplete-mode icomplete-mode
                          "Command Completion Clues" "Completion Clues %s")
    'all-options-separator))


;; Remove some default bindings.
;;;@@@Emacs20 (global-unset-key [menu-bar help emacs-news])
;;;@@@Emacs20 (global-unset-key [menu-bar help emacs-faq])

;;;@@@Emacs20 (define-key menu-bar-help-menu [help-on-click]
;;;@@@Emacs20   '("What's This?..." . help-on-click/key))       ; Defined in `help+.el'.
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [help-for-help]
;;;@@@Emacs20   '("Help on Help..." . help-for-help) 'help-on-click)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-menubar]
;;;@@@Emacs20   '("Describe Menu Bar" . describe-menubar) 'help-for-help)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-help-click] '("--")
;;;@@@Emacs20   'view-lossage)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-mode]
;;;@@@Emacs20   '("Describe Mode" . describe-mode) 'separator-help-click)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-syntax]
;;;@@@Emacs20   '("Describe Mode Syntax" . describe-syntax) 'describe-mode)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-variable]
;;;@@@Emacs20   '("Describe Variable..." . describe-variable) 'describe-syntax)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [edit-options]
;;;@@@Emacs20   '("Show/Set Variables" . edit-options) 'describe-variable)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-function]
;;;@@@Emacs20   '("Describe Function..." . describe-function) 'edit-options)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [where-is]
;;;@@@Emacs20   '("Where is Command..." . where-is) 'describe-function)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [Info-goto-emacs-command-node]
;;;@@@Emacs20   '("`Info' on Command..." . Info-goto-emacs-command-node) 'where-is) ; Defined in `info.el'.
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [describe-key]
;;;@@@Emacs20   '("Describe Key/Menu..." . describe-key) 'Info-goto-emacs-command-node)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [Info-goto-emacs-key-command-node]
;;;@@@Emacs20   '("`Info' on Key/Menu..." . Info-goto-emacs-key-command-node) 'describe-key) ; Defined in `info.el'.
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [list-keybindings]
;;;@@@Emacs20   '("Show Key/Menu Bindings" . describe-bindings)
;;;@@@Emacs20   'Info-goto-emacs-key-command-node)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-manuals] '("--")
;;;@@@Emacs20   'apropos-doc%%%%%%%%%%%%%)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [info] '("`Info' (online manuals)" . info) ; In `info+.el'.
;;;@@@Emacs20   'separator-manuals)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [man]
;;;@@@Emacs20   '("Unix Manual..." . manual-entry) 'info)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-emacs] '("--") 'man)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [emacs-tutorial]
;;;@@@Emacs20   '("Emacs Tutorial" . help-with-tutorial) 'separator-emacs)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [emacs-faq]
;;;@@@Emacs20   '("Emacs FAQ" . view-emacs-FAQ) 'emacs-tutorial)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [emacs-news]
;;;@@@Emacs20   '("Emacs Changes" . view-emacs-news) 'emacs-faq)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-lispdoc] '("--")
;;;@@@Emacs20   'emacs-news)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [finder-by-keyword]
;;;@@@Emacs20   '("Lisp Libraries by Keyword" . finder-by-keyword) ; Defined in `finder.el'.
;;;@@@Emacs20   'separator-lispdoc)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [locate-library] ; Defined in `help.el'.
;;;@@@Emacs20   '("Locate Lisp Library..." . locate-library) 'finder-by-keyword)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [emacs-Lisp-News]
;;;@@@Emacs20   '("Emacs Lisp Changes" . view-emacs-lisp-news) ; Defined in `help.el'.
;;;@@@Emacs20   'locate-library)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-about] '("--")
;;;@@@Emacs20   'emacs-Lisp-News)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [emacs-version]
;;;@@@Emacs20   '("Show Version" . emacs-version) 'separator-about)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [report-emacs-bug]
;;;@@@Emacs20   '("Send Bug Report..." . report-emacs-bug) 'emacs-version)
;;--------------------
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [separator-help-buffer] '("--")
;;;@@@Emacs20   'report-emacs-bug)
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [save-*Help*-buffer]
;;;@@@Emacs20   '("Save *Help* Buffer" . save-*Help*-buffer) 'separator-help-buffer) ; In `help+.el'.
;;;@@@Emacs20 (define-key-after menu-bar-help-menu [show-*Help*-buffer]
;;;@@@Emacs20   '("Show *Help* Buffer" . show-*Help*-buffer) 'save-*Help*-buffer) ; In `frame-cmds.el' (defsubst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `menu-bar+.el' ends here
;;; menu-bar-plus.el ends here
