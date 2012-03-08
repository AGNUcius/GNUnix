(defvar assoclist '((x 1) (y 5) (z 1))

(car (cdr (assq 'y assoclist)))


(defun replace-from-hash (hashassoc)
  "replace each car (first element) in every sublist with that sublist's second element in the current buffer after point."
  (interactive)
  (while (re-search-forward REGEXP nil t)
  (replace-match TO-STRING nil nil))
