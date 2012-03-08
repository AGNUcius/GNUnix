(defun vlc ()
  (interactive)
  (start-process "vlc"
				 (get-buffer-create "*vlc*")
				 "vlc"
				 "--intf" "dummy"
				 (dired-get-filename nil t)))
