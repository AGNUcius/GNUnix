 (eval-after-load "em-ls"
   '(progn
      (defvar ted-eshell-ls-keymap (make-sparse-keymap))

      (define-key ted-eshell-ls-keymap (kbd "RET")
        'ted-eshell-ls-find-file)
      (define-key ted-eshell-ls-keymap (kbd "<return>")
        'ted-eshell-ls-find-file)
      (define-key ted-eshell-ls-keymap [(mouse-2)] (lambda (event)
        (interactive "e")
        (goto-char (posn-point (event-end event)))
        (ted-eshell-ls-find-file)))

      (defun ted-eshell-ls-find-file ()
        (interactive)
        (find-file (buffer-substring-no-properties
                    (previous-single-property-change (point) 'help-echo)
                    (next-single-property-change (point) 'help-echo))))

      (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
        ""
        (add-text-properties 0 (length ad-return-value)
                             (list 'help-echo "RET, mouse-2: visit this file"
                                   'mouse-face 'highlight
                                   'keymap ted-eshell-ls-keymap)
                             ad-return-value)
        ad-return-value)))


;; (defun ted-eshell-ls-find-file ()
;;   (interactive)
;;   (let ((fname (buffer-substring-no-properties
;; 				(previous-single-property-change (point) 'help-echo)
;; 				(next-single-property-change (point) 'help-echo))))
;; 	;; Remove any leading whitespace, including newline that might
;; 	;; be fetched by buffer-substring-no-properties
;; 	(setq fname (replace-regexp-in-string "^[ \t\n]*" "" fname))
;; 	;; Same for trailing whitespace and newline
;; 	(setq fname (replace-regexp-in-string "[ \t\n]*$" "" fname))
;; 	(cond
;; 	 ((equal "" fname)
;; 	  (message "No file name found at point"))
;; 	 (fname
;; 	  (find-file fname)))))

