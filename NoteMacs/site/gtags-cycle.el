;;http://www.emacswiki.org/emacs/CyclingGTagsResult

;; GNU Global is pretty cool as an Emacs tagging tool. However, its gtags.el package integrating GNU Global inside Emacs doesn't support cycling through multiple tag results. I whipped up some Elisp to do the trick. It supports cycling all kinds of GTAGS result: tag, rtag, symbol, etc. Here's in my .emacs file.

(require 'gtags)

(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))

;; Here's my key binding for using GNU Global.

(global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
(global-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
(global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
(global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.

;; - WilliamWong
