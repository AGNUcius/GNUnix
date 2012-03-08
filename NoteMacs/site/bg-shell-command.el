;;; bg-shell-command.el
;;;
;;; Rel:v-1-47
;;;
;;; Copyright (C) 1997, 1998,2004 Thien-Thi Nguyen
;;; This file is part of ttn's personal elisp library, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Description: Run a shell command in the background, renaming buffer.

;;;###autoload
(defun bg-shell-command (cmd &optional watch)
  "Do shell CMD in background, renaming controlling buffer.
If optional arg WATCH non-nil, switch to that buffer, otherwise show nothing."
  (interactive "sShell command (to be backgrounded): \np")
  (let (buf)
    (save-excursion
      (save-window-excursion
		(shell-command (concat cmd "&"))
;;		(start-process "cmd.exe" "zz" "cmd.exe")
		(set-buffer "*Async Shell Command*")
		(setq buf (rename-buffer (concat "*bg job* " cmd) t))
		))
	(with-current-buffer buf 
	  (insert (concat cmd ": finished.")))
    (when watch
      (switch-to-buffer buf))))

(provide 'bg-shell-command)

;;; bg-shell-command.el ends here
