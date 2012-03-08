;; C-x C-f /some/nonexisting/file/name RET
;; C-u C-\ chinese-py RET
;; nihao                   (enter Chinese here)
;; C-x RET c utf-8 RET
;; C-x C-s

(defun my-fontset-menu ()
      (interactive)
      (x-popup-menu
      `((0 0) ,(selected-frame)) 
      (append x-fixed-font-alist
        (list (generate-fontset-menu)))))

;;;from w32-win.el:
;; (defun mouse-set-font (&rest fonts)
;;   "Select a font.
;; If `w32-use-w32-font-dialog' is non-nil (the default), use the Windows
;; font dialog to get the matching FONTS. Otherwise use a pop-up menu
;; \(like Emacs on other platforms) initialized with the fonts in
;; `w32-fixed-font-alist'."
;;   (interactive
;;    (if w32-use-w32-font-dialog
;;        (let ((chosen-font (w32-select-font (selected-frame)
;; 					   w32-list-proportional-fonts)))
;; 	 (and chosen-font (list chosen-font)))
;;      (x-popup-menu
;;       last-nonmenu-event
;;     ;; Append list of fontsets currently defined.
;;       ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
;;       (if (fboundp 'new-fontset)
;;       (append w32-fixed-font-alist (list (generate-fontset-menu)))))))
;;   (if fonts
;;       (let (font)
;; 	(while fonts
;; 	  (condition-case nil
;; 	      (progn
;;                 (setq font (car fonts))
;; 		(set-default-font font)
;;                 (setq fonts nil))
;; 	    (error (setq fonts (cdr fonts)))))
;; 	(if (null font)
;; 	    (error "Font not found")))))




;to determine font from GUI selection:
;(insert(prin1-to-string(w32-select-font)))

