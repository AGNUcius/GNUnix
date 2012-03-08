;;; gud-keys.el --- Use DevStudio/VisualStudio keys in gdba

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;: > run
(define-key gud-minor-mode-map [f5] 'gud-cont)
(define-key gud-minor-mode-map [f8] 'gud-step)
(define-key gud-minor-mode-map [f9] 'gud-break)
(define-key gud-minor-mode-map [shift f9] 'gud-tbreak)

(define-key gud-minor-mode-map [f10] 'gud-next)
(define-key gud-minor-mode-map [f11] 'gud-step)
(define-key gud-minor-mode-map [S-f11] 'gud-finish)

(provide 'gud-keys)
;;; gud-keys.el ends here
