;;; next-token.el --- alternative to `forward-word'

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Patrick Anderson <panderson@none.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; alternative to `forward-word'

;;; Code:

;;;###autoload
(defun forward-token (arg)
  "alternative to `forward-word'"
  (interactive "p")
  (if (natnump arg)
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil 'move)
	  (skip-syntax-backward "w_"))
      (setq arg (1+ arg)))))

;(define-key global-map [(meta f)] 'forward-token)
;(define-key global-map [(meta b)] 'backward-token)

(provide 'next-token)
;;; next-token.el ends here
