;;; popular-options.el --- popularly changed options
;;; Commentary:

;; type
;; M-x eval-buffer RET
;; M-x popular-options RET
;; this will create a "customization" buffer used to make permanent changes to emacs behavior

;;; Code:

(defgroup popular-options nil "popular options"
  :group 'emacs)

(mapcar (lambda (var)
          (custom-add-to-group 'popular-options var 'custom-variable))
        '(
          global-font-lock-mode             ; Very useful.
          generic-define-unix-modes         ; Enable all generic modes, in fact!
          show-paren-mode                   ; Very useful.
          auto-compression-mode             ; Not dangerous, nice to have.
          confirm-kill-emacs                ; slippery fingers
          column-number-mode                ; performance hit
          user-mail-address                 ; Default not correct on all systems that are not mail hosts?
          user-full-name                    ; Default is correct on my system.

;;;Option is popular but dangerous.
          file-name-shadow-mode             ; you hit '/' by mistake and now have to retype the default directory. -- LathI
          iswitchb-mode                     ; "confusing and frightening firework of colours and characters in his or her eyes."
          icomplete-mode                    ; nice to have.  danger similar to iswitchb.
          dired-recursive-copies            ; dangerous but powerful
          dired-recursive-deletes           ; dangerous but powerful
          enable-recursive-minibuffers      ; This is confusing for newbies, no?
          hi-lock-mode                      ; fx dislikes it

;;;Option is devisive
          blink-cursor                      ; Some may think it bandwidth intensive, others just prefer non-blinking cursors.
          pc-selection-mode                 ; Alternative for working with the region.
          transient-mark-mode               ; Alternative for working with the region.
          delete-selection-mode             ; Alternative for working with the region.
          cua-mode                          ; from KimStorm (new in 21.4), similar to pc-selection-mode, but more intrusive
          ido-mode                          ; from KimStorm (new in 21.4), similar iswitchb-mode, but more intrusive
          show-trailing-whitespace          ; Informative.
          completion-ignored-extensions     ; I did not understand why log files never completed...
          view-diary-entries-initially      ; Cool.
          global-hl-line-mode               ; Interesting for newbies.
          ibuffer                           ; the buffer-menu is better than buffer-list, but ibuffer is even better.
          desktop-enable                    ; Not sure wether this is meaningful -- only if the default is to use the desktop from the user's home directory.
          apropos-do-all                    ; I did not know about it!
          winner-mode                       ; I did not know about it!

          x-stretch-cursor                  ; Ugly!  :
          font-lock-maximum-size            ; I do not care for this.

          display-time-mode                 ; Clutters the mode-line.
          ange-ftp-dumb-unix-host-regexp    ; What would a good default value be?
          next-line-add-newlines            ; What is this?  If this is the variable that controls wether moving down beeps or adds a newline, do not change it, because it was just changed in the last release, I think.
          tab-width                         ; Many people do not want to change this, they want to change the indentation engines for C, Perl, Java, etc.  The answer to those questions is more complicated.
          scroll-conservatively             ; Should be > 0
          scroll-preserve-screen-position   ; The default is confusing for newbies.
          ))

(customize-group 'popular-options)

;;; popular-options.el ends here
