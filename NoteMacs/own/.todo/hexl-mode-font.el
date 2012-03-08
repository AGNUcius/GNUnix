(defvar hexl-font-lock-keywords
  '(;;
	("0"
	 (1 font-lock-string-face))))


(add-hook 'hexl-mode-hook
		  '(lambda () (set (make-local-variable 'font-lock-defaults) hexl-font-lock-keywords)))