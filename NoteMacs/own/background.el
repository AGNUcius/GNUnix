;;; background.el -- based on bg-shell-command.el by Thien-Thi Nguyen

(defun bg-shell-command (cmd &optional watch)
  "Do shell CMD in background, renaming controlling buffer.
If optional arg WATCH non-nil, switch to that buffer, otherwise show nothing."
  (interactive "sbackground shell command: \np")
  (save-window-excursion
	(with-current-buffer 
		(generate-new-buffer
		 (generate-new-buffer-name (concat "* background: " cmd " *")))
	  (shell-command (concat cmd "&")))))

;; (defun bg-shell-command (cmd)
;;   "Do shell CMD in background, renaming controlling buffer.
;; If optional arg WATCH non-nil, switch to that buffer, otherwise show nothing."
;;   (interactive "sbackground shell command: \n")
;;   (start-process
;;    cmd
;;    (generate-new-buffer
;; 	(generate-new-buffer-name (concat "* background: " cmd " *")))
;;    cmd
;;    "&"))
