;this is used by ../site-bin/aim.bat to make a separate emacs frame containing an aim session
(setq load-path
	  (append ["~/site-lisp"] ["~/site-lisp/tnt"] load-path))
(require 'tnt)
(tnt-show-buddies)
(menu-bar-mode nil)
(scroll-bar-mode nil)
;(setq visible-bell t)
(setq frame-title-format "%b")			;show name of window in frame title bar