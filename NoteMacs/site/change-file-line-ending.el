;; http://ergoemacs.org/emacs/elisp_convert_line_ending.html

(defun change-file-line-ending (fpath lineEndingStyle)
  "Change file's newline character.
 「fpath」 is full path to file.
 「lineEndingStyle」 is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.
"
  (let (mybuffer
        (bufferOpened-p (get-file-buffer fpath))
        )
    (if bufferOpened-p
        (progn (with-current-buffer bufferOpened-p (set-buffer-file-coding-system lineEndingStyle) (save-buffer) ))
      (progn
        (setq mybuffer (find-file fpath))
        (set-buffer-file-coding-system lineEndingStyle)
        (save-buffer)
        (kill-buffer mybuffer) ) ) ) )

(defun change-file-line-ending-style (fileList lineEndingStyle)
  "Change current file or dired marked file's newline convention.
When called in lisp program, “lineEndingStyle” is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.
"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)) )
    (ido-completing-read "Style:" '("Unix" "Mac OS 9" "Windows") "PREDICATE" "REQUIRE-MATCH"))
   )
  (let* (
         (nlStyle
          (cond
           ((equal lineEndingStyle "Unix") 'unix)
           ((equal lineEndingStyle "Mac OS 9") 'mac)
           ((equal lineEndingStyle "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." ))
           ))
         )
    (mapc
     (lambda (ff) (change-file-line-ending ff nlStyle))
     fileList)) )
