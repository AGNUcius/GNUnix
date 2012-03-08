;; from http://NSIS.sf.net/Nsi-mode_for_emacs
(defsubst nsi-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line    eol  -- end of line           bod  -- beginning of def or class
  eod  -- end of def or class  bob  -- beginning of buffer   eob  -- end of buffer
  boi  -- back to indentation  bos  -- beginning of statement
This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (nsi-beginning-of-def-or-class))
     ((eq position 'eod) (nsi-end-of-def-or-class))
     ;; Kind of funny, I know, but useful for py-up-exception.
     ((eq position 'bob) (beginning-of-buffer))
     ((eq position 'eob) (end-of-buffer))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (nsi-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))
