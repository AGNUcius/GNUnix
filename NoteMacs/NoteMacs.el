;;; NoteMacs.el --- Make Emacs a little easier to use.

;; Copyright (C): Social Sufficiency Coalition

;; Author: Patrick Anderson <AGNUcius@Gmail.com>

;; This file is Free Software.
;; You can Use, Modify, Copy and Share it under the terms
;; of the GNU Affero General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;;; Commentary:
;;  NoteMacs is an attempt to tame the One True Editor.
;; 0. NoteMacs shall not bother current church members.
;; 1. NoteMacs seeks unification and orthogonalization.
;; 2. NotEmacs pleases some CUA/Microsoft expectations.
;; 3. NoteMacs shall be verbose and obvious for newbie.
;; 4. NoteMacs provides advanced options for old-timer.

;;; Installation:
;; cd ~/
;; git clone git://github.com/AGNUcius/GNUnix
;; mv GNUnix/* .
;; mv GNUnix/.* .


;;; Customizations:
;; Change the following values directly by replacing the value 't' with the value 'nil', or vice-versa.  These are not (yet) `defcustom' as maybe they should be.
(defconst NoteMacs-NotEmacs nil "Make Emacs behave like Not Emacs including CUA keys, no infinite undo, and many conflicts with Emacs documentation.")
(defconst NoteMacs-unified-keys t "Make keys more consistient across modes.  Transgresses letter but hopefully not spirit.")
(defconst NoteMacs-apprentice t "Enable medium risk keys and behavior.")
(defconst NoteMacs-old-timer t "Enable advanced keys and behavior.")


;;; Code:
(add-to-list 'load-path "~/NoteMacs/own")
(add-to-list 'load-path "~/NoteMacs/site")

;; android dev
(defun android-dev () (interactive)
  (setq eshell-path-env (concat eshell-path-env ":/home/user/work/android/adt/sdk/tools:/home/user/work/android/adt/sdk/platform-tools")))

(setq debug-on-error nil)

;; Mac OS X stuff
(when (eq system-type 'darwin)
 (setq mac-command-modifier 'meta))

;; Go to "System Preferences"|Keyboard|Keyboard|Modifier Keys
;; Change 'Caps-Lock' to send 'Control'
;; Change 'Option' to send 'Command'
;; Change 'Command' to send 'Option'

;; $ cp $FILE ~/keys-binary.plist
;; $ plutil -convert xml1 -o ~/keys-xml.plist ~/keys-binary.plist
;; Open the resulting XML file and locate the com.apple.keyboard.modifiermapping key. Its value is an array containing several entries, each in turn containing HIDKeyboardModifierMappingSrc and HIDKeyboardModifierMappingDst keys. The values of those keys are described in the table above. Make the changes you wish to make, and save your work.

;; Convert the file back to the binary property list format, and put it back in the correct location:
;; $ plutil -convert binary1 -o ~/keys-binary.plist ~/keys-xml.plist
;; $ cp ~/keys-binary.plist $FILE


(autoload 'lens-mode "lens.el" "" t)
(autoload 'lens "lens.el" "" t)
(autoload 'xml-beautify "xml-control" "unwind and indent" t)
(autoload 'sic-connect "sic" "simple IMAP client" t)
(autoload 'ewb "ewb" "Emacs Web Browser" t)
(autoload 'hands "hands" "image preview" t)

(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(define-key global-map [(meta control y)] 'browse-kill-ring)

(autoload 'findr "findr" "Find file name." t)
(define-key global-map [(meta control S)] 'findr)
(autoload 'findr-search "findr" "Find text in files." t)
(define-key global-map [(meta control s)] 'findr-search)
(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map [(meta control r)] 'findr-query-replace)

(autoload 'hexview-find-file "hexview-mode" nil t)
(autoload 'time-insert "time-insert" nil t)
(autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'javascript-mode "javascript" nil t)
(autoload 'tabbar-mode "tabbar" nil t)
(autoload 'powershell-mode "powershell-mode" nil t)
(autoload 'w3m "w3m" nil t)

;;(require 'generic-x)
(autoload 'bat-generic-mode "generic-x" "" t)
(autoload 'ini-generic-mode "generic-x" "" t)
(autoload 'inf-generic-mode "generic-x" "" t)
(autoload 'rc-generic-mode "generic-x" "" t)
(autoload 'reg-generic-mode "generic-x" "" t)
(autoload 'rul-generic-mode "generic-x" "" t)
(autoload 'hosts-generic-mode "generic-x" "" t)
(autoload 'apache-conf-mode "generic-x" "" t)
(autoload 'apache-log-generic-mode "generic-x" "" t)
(autoload 'apache-conf-generic-mode "generic-x" "" t)
(autoload 'apache-log-generic-mode "generic-x" "" t)
(autoload 'samba-generic-mode "generic-x" "" t)
(autoload 'hosts-generic-mode "generic-x" "" t)
(autoload 'fvwm-generic-mode "generic-x" "" t)
(autoload 'x-resource-generic-mode "generic-x" "" t)
(autoload 'alias-generic-mode "generic-x" "" t)
(autoload 'inetd-conf-generic-mode "generic-x" "" t)
(autoload 'etc-services-generic-mode "generic-x" "" t)
(autoload 'etc-passwd-generic-mode "generic-x" "" t)
(autoload 'etc-fstab-generic-mode "generic-x" "" t)
(autoload 'io-mode "io-mode" "" t)

(autoload 'rfcview-mode "rfcview" "" t)
(autoload 'css-mode "css-mode-min")
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'htmlize-file "htmlize" "" t)
(autoload 'csharp-mode "csharp-mode" "csharp-mode" t)
(autoload 'malyon "malyon" "malyon" t)
(autoload 'ascii-display "ascii" "Toggle on ASCII code display." t)


(load "dired-x")
(load "dired-aux")

(define-key global-map [(meta o)] 'other-window)
(define-key global-map [(shift meta o)] 'dired-omit-mode) ;;alternate omit

(define-key ctl-x-map [(p)] 'proced)
(defun kill-process(proc) (interactive "sProcess Name: ") (delete-process proc))
(define-key ctl-x-map [(P)] 'kill-process)

(define-key global-map [(C-M-tab)] 'bury-buffer)
(define-key ctl-x-map [(B)] 'bury-buffer) ;the above fails on some terminals

(define-key global-map [(meta ?/)] 'hippie-expand)
;;(define-key global-map [(meta ??)] (lambda () (hippie-expand-undo -1)))

(define-key global-map [(meta Q)]
  (lambda ()
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil))))

(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

;;(setq w32-lwindow-modifier 'hyper)
;; w32-pass-alt-to-system
;; w32-alt-is-meta
(define-key global-map [(meta g)] 'goto-line) ;; default in CVS Emacs?

;; who wrote this?
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ;;if just past a paren...
        ;; save-excursion
        ;; backward-char
        ;; ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((message "no paren here")))
  )

(define-key global-map [(control \()] 'match-paren)


(defadvice isearch-exit (after my-goto-match-beginning activate) ;this put the point at the _beginning_ of the match
  "Go to beginning of match."
  (when isearch-forward (goto-char isearch-other-end)))


;; from http://www.emacswiki.org/cgi-bin/wiki.pl?EshellFunctions
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map "\C-a"
              (lambda ()
                (interactive)
                (let ((p (point)))
                  (eshell-bol)
                  (if (= p (point))
                      (beginning-of-line)))))))

(defun delete-file-of-buffer(b)
  "delete the file associated with this buffer (if present)"
  (interactive "b")
  (delete-file (buffer-file-name)))

(defun delete-preceding-whitespace ()
  (interactive)
  (query-replace-regexp "^\\(   \\| \\)+" ""))

(defun pan-up () (interactive) (scroll-down -1) (next-line))
(defun pan-down () (interactive) (scroll-down 1) (next-line -1))
(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))

(if NoteMacs-unified-keys
    (progn
	  (define-key dired-mode-map [(delete)] 'dired-do-delete)
	  (define-key dired-mode-map "\M-\r" 'browse-url-of-dired-file)
	  (define-key dired-mode-map "\M-o" 'other-window)
	  (define-key dired-mode-map "w" 'woman-dired-find-file)
	  (define-key dired-mode-map " " 'scroll-up)
	  (define-key dired-mode-map "u" 'scroll-down)
	  (define-key dired-mode-map "l" 'recenter)
	  (define-key dired-mode-map "c" 'dired-unmark)	;; c clear mark
	  (define-key dired-mode-map "q" 'kill-current-buffer)
	  (define-key dired-mode-map "U" 'dired-up-directory)

	  ;;kill buffers instead of burying them
	  (fset 'quit-window 'kill-current-buffer)

      ;; CUA OpenFile.  Also touched in `dired' (see below)
      (define-key global-map [(control o)] 'ffap)

      (require 'view) ;;autoload doesn't work here
	  (define-key view-mode-map " " 'scroll-up)	  ;;page down
	  (define-key view-mode-map "u" 'scroll-down) ;;page up
	  (define-key view-mode-map [(backspace)] 'scroll-down) ;; page up
	  (define-key view-mode-map "q" 'kill-current-buffer)

	  ;; Act as though CTRL is being held:
	  (define-key view-mode-map "v" 'scroll-up) ;;page down
	  (define-key view-mode-map "k" 'kill-line)
	  (define-key view-mode-map "l" 'recenter)
	  (define-key view-mode-map "a" 'beginning-of-line)
	  (define-key view-mode-map "e" 'end-of-line)
	  (define-key view-mode-map [(home)] 'beginning-of-buffer)
	  (define-key view-mode-map [(end)] 'end-of-buffer)
	  (define-key view-mode-map "n" 'pan-up)
	  (define-key view-mode-map "p" 'pan-down)

	  ;;climb Up
	  (define-key view-mode-map [(U)]
		(lambda ()
		  (interactive)
		  (dired default-directory)))

	  ;;revert
	  (define-key view-mode-map "g"
		(lambda ()
		  (interactive)
		  (revert-buffer nil t t)
		  (toggle-read-only nil)))

	  (add-hook 'nxml-mode-hook
				(lambda ()
				(define-key nxml-mode-map [(meta h)] 'backward-kill-word)))

	  ;;These hooks are executed every time we enter that mode!
	  (add-hook 'archive-mode-hook
				(lambda ()
				  (define-key archive-mode-map " " 'scroll-up)
				  (define-key archive-mode-map "u" 'scroll-down)
				  (define-key archive-mode-map "q" 'kill-current-buffer)))

	  (add-hook 'completion-list-mode-hook
				(lambda ()
				  (define-key completion-list-mode-map [(q)] 'kill-current-buffer)))

	  (add-hook 'custom-mode-hook
				(lambda ()
				  (define-key custom-mode-map [(q)] 'kill-current-buffer)))

	  (add-hook 'woman-mode-hook
				(lambda ()
				  (define-key woman-mode-map [(u)] 'scroll-down)))

	  (add-hook 'Info-mode-hook
				(lambda ()
				  (define-key Info-mode-map [(u)] 'Info-scroll-down)
				  (define-key Info-mode-map [(U)] 'Info-up)
				  (define-key Info-mode-map [(backspace)] 'Info-last)
				  (define-key Info-mode-map [(meta left)] 'Info-last)
				  (define-key Info-mode-map [(meta n)] nil) ;unmask my global key
				  (define-key Info-mode-map [(n)] 'next-line)
				  (define-key Info-mode-map [(p)] 'previous-line)
				  (define-key Info-mode-map [(control I)] 'Info-prev-reference)))

	  (add-hook 'tar-mode-hook
				(lambda ()
				  (define-key tar-mode-map " " 'scroll-up)
				  (define-key tar-mode-map "u" 'scroll-down)
				  (define-key tar-mode-map "q" 'kill-current-buffer)))

	  (if (file-exists-p "~/NoteMacs/own/w32-dev.el")
		  (require 'w32-dev) (message "w32-dev unavailable"))
	  (if (file-exists-p "~/NoteMacs/own/edebug-control.el")
		  (require 'edebug-control) (message "edebug-control unavailable"))
))

(if NoteMacs-apprentice
    (progn
      (define-key global-map "\M-\r" 'browse-url-at-point)
      (autoload 'skeleton-pair-insert-maybe "skeleton" "" t)
      (setq skeleton-pair t)
      (define-key global-map [(\()] 'skeleton-pair-insert-maybe)
      (define-key global-map [(\[)] 'skeleton-pair-insert-maybe)
      (define-key global-map [({)] 'skeleton-pair-insert-maybe)
      (define-key global-map [(<)] 'skeleton-pair-insert-maybe)
      ;;(define-key global-map [(\")] 'skeleton-pair-insert-maybe)
	  (add-to-list 'same-window-buffer-names "*Buffer List*")
      (define-key Buffer-menu-mode-map " " 'scroll-up)
      (define-key Buffer-menu-mode-map "u" 'scroll-down)
      (define-key Buffer-menu-mode-map "c" 'Buffer-menu-unmark)
	  (autoload 'dired-efap "dired-efap" "Make current `dired' filename editable." t)
	  (define-key dired-mode-map [(f2)] 'dired-efap)))

;;; Enable advanced keys and behavior.
(if NoteMacs-old-timer
    (progn
	  (define-key global-map [(f4)] 'next-error)
	  (define-key global-map [(shift f4)]
		(lambda ()
		  (interactive)
		  (next-error -1)))

      ;; If you have the memory this speeds things up?
      ;; (setq-default gc-cons-threshold 50000000) ;; This looks strange and dangerous

      ;; Make C-h the traditional 'Backspace'
      (keyboard-translate ?\C-h ?\C-?) ;;was: Help
      (define-key global-map [(control H)] 'help) ;; hold SHIFT or use F1 instead

      ;; Like C-h, but for words
      (define-key global-map [(meta h)] 'backward-kill-word) ;;was: mark-paragraph

      ;; Could cause accidents
      (fset 'yes-or-no-p 'y-or-n-p)

      ;; Informative, but maybe confusing
      (icomplete-mode t)

      (require 'dired-sort-map) ;;messes with the  s  key in `dired'

      ;;This stops `minibuffer-complete-word' from swallowing some
      ;;chars, especially '-', but maybe this is not what we want?
      (defalias 'minibuffer-complete-word 'minibuffer-complete)

      ;;This is a bit like using `find-file-literally' on every file
      ;;(setq-default enable-multibyte-characters nil)

      (define-key global-map [(control K)] 'kill-current-buffer)
      (define-key global-map [(control R)] 'query-replace-regexp)
      (define-key global-map [(control J)] 'join-line)

      ;;This is unsafe because the resulting buffer is not backed by a
      ;;file.  How do I use `buffer-offer-save' to warn?
      (defun create-new-buffer()
        "Create a new 'fileless' buffer"
        (interactive)
        (switch-to-buffer
         (generate-new-buffer
          (generate-new-buffer-name "Untitled"))))

      (define-key ctl-x-map [(control n)] 'create-new-buffer)

      (define-key global-map [(control z)] 'eshell)
      (define-key ctl-x-map [(control z)] 'eshell)

      (define-key global-map [(control N)] 'pan-up)
      (define-key global-map [(control down)] 'pan-up)
      (define-key global-map [(control P)] 'pan-down)
      (define-key global-map [(control up)] 'pan-down)

      (define-key global-map [(meta n)] 'forward-paragraph)
      (define-key global-map [(meta p)] 'backward-paragraph)

      ;;calendar
      (define-key ctl-x-map [(l)] 'calendar)

      (defun diary-prepend-entry (file)
        (find-file file) ;;open to our diary file - which is a list of files corresponding to monthly divisions
        (beginning-of-buffer) ;;
		(lens-follow) ;;go to the most recent month
        (beginning-of-buffer)
		(toggle-read-only nil)
		(newline 2)
		(forward-line -2)
        (time-insert))

      (add-hook
       'calendar-load-hook
       (lambda ()
         (progn
           (define-key calendar-mode-map "it" (lambda nil (interactive) (find-file "~/doc/.txt/todo")))
           ;;insert daily
           (define-key calendar-mode-map "id"
             (lambda nil (interactive)
               (diary-prepend-entry diary-file)))
           )))

      (add-hook 'ediff-load-hook
                (lambda ()
                  (defun ediff-window-display-p nil))) ;force single frame
      ))

;;;These settings are for those accustomed to Not Emacs behavior.
;; This section is below other NoteMacs levels to ensure priority for key assignments (such as C-z) that are defined in both.
(if NoteMacs-NotEmacs
    (progn
      ;; ESC means "Cancel" ;could use C-g instead. was: alternate to Meta
	  ;;        (setq w32-quit-key 27) ;;maybe needed on windows?
      (global-set-key [escape] 'keyboard-quit)

      (if (< emacs-major-version 22)
          (progn (require 'cua) (CUA-mode t)) ;;use old version
		(cua-mode))							  ;;part of emacs now

      ;; C-y redo ;infinite undo instead. was: yank
      (if (file-exists-p "~/NoteMacs/site/redo.el")
          (progn
            (require 'redo)
            (define-key global-map [(control y)] 'redo))
		(message "redo unavailable"))

      ;; C-a select all ;use C-xh instead. was: beginning of line
      (define-key global-map [(control a)] 'mark-whole-buffer)

      ;; C-w close ;use C-f4 or C-K instead. was: cut
      (define-key global-map [(control w)] 'kill-current-buffer)
      (define-key global-map [(control f4)] 'kill-current-buffer)

      ;; C-s save ;use C-xC-s instead. was: incremental search
      (define-key global-map [(control s)] 'save-buffer)

      ;; C-f find ;use C-s instead. was: forward-char
      (define-key global-map [(control f)] 'isearch-forward)
      (define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)
      (define-key isearch-mode-map [(f3)] 'isearch-repeat-forward)

      ;; C-n new ;use C-xC-f or C-xC-n instead. was: next-line
      ;;  (define-key global-map [(control n)] 'create-new-buffer)

      ;; right=2, middle=3;  Is this what NotEmacs users expect?
      (setq w32-swap-mouse-buttons t)

      ;; M-f4 exit Emacs;use C-x C-c instead. was: `undefined'
      (define-key global-map [(meta f4)] 'save-buffers-kill-emacs) ;;this may work anyway if your window manager is catching this key and sending a 'quit' to Emacs.
	  (define-key dired-mode-map [(f5)] 'revert-buffer)
	  (define-key dired-mode-map [(control o)] 'ffap)
      ))

;;; Cosmetics
;; Frame Title (Also see `mode-line-format')
(setq frame-title-format `(,(user-login-name) "@" ,(system-name) " " global-mode-string " %f" ))


;;; Where to put this?
;;these require c-default-style be set to "user"
(c-set-offset 'case-label '+)
(c-set-offset 'inline-open 0)
(c-set-offset 'substatement-open 0)

;; C-TAB cycle through open buffers/windows
(if (file-exists-p "~/NoteMacs/site/pc-bufsw.el")
    (progn
      (require 'pc-bufsw)
      (pc-bufsw::bind-keys [C-tab] [C-S-tab]))
  (message "C-Tab buffer switching unavailable"))


;;; Customized variables
;;  press f1 v RET  while over any variable name
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-scroll-prefer-subnodes nil)
 '(ahk-syntax-directory "/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax/")
 '(ange-ftp-dumb-unix-host-regexp "netbsd\\|freeshell")
 '(ange-ftp-generate-anonymous-password "none@aol.com")
 '(appt-message-warning-time 2)
 '(apropos-do-all t)
 '(archive-zip-use-pkzip nil)
 '(ascii-window-size 4)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-image-file-mode t nil (image-file))
 '(auto-insert-mode t nil (autoinsert))
 '(blink-cursor-interval 0.333)
 '(blink-matching-paren-dont-ignore-comments t)
 '(blink-matching-paren-on-screen nil)
 '(bookmark-bmenu-toggle-filenames nil)
 '(bookmark-sort-flag nil)
 '(browse-kill-ring-quit-action (quote kill-and-delete-window))
 '(c-default-style "user")
 '(c-echo-syntactic-information-p t)
 '(c-mode-common-hook (quote (hs-minor-mode)))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-file "~/NoteMacs/NoteMacs.el")
 '(delete-selection-mode t nil (delsel))
 '(diary-file "~/doc/.txt/diary")
 '(diary-hook (quote (appt-make-list)))
 '(dired-at-point-require-prefix t)
 '(dired-omit-files "^\\.\\|^#")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(dired-view-command-alist nil)
 '(display-time-day-and-date t)
 '(display-time-format "%l:%M %p  %A, %B %e %Y")
 '(display-time-mode t nil (time))
 '(dmoccur-maximum-size 5000)
 '(ediff-ignore-similar-regions t t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode checkdoc-minor-mode)))
 '(enable-recursive-minibuffers t)
 '(eol-mnemonic-mac ":")
 '(eol-mnemonic-undecided "?")
 '(eol-mnemonic-unix "/")
 '(fill-column 54)
 '(frame-font-string "-outline-NSimSun-normal-r-normal-normal-29-217-96-96-c-*-iso8859-1")
 '(garbage-collection-messages t)
 '(gdb-show-main t)
 '(generic-define-unix-modes t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t nil (hl-line))
 '(gud-cdb-directories (quote (".\\" "..\\")))
 '(gud-chdir-before-run nil)
 '(hi-lock-mode t t (hi-lock))
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-dabbrev-all-buffers try-complete-file-name try-expand-list try-expand-line try-complete-lisp-symbol)))
 '(hl-line-face (quote trailing-whitespace))
 '(hs-isearch-open t)
 '(ibuffer-expert nil)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-use-other-window t)
 '(iswitchb-mode t nil (iswitchb))
 '(jit-lock-chunk-size 5000)
 '(jit-lock-stealth-load 50)
 '(lazy-highlight-cleanup t)
 '(lazy-highlight-initial-delay 0)
 '(lazy-highlight-max-at-a-time nil)
 '(line-move-visual nil)
 '(list-command-history-max 128)
 '(ls-lisp-verbosity nil)
 '(mail-self-blind t)
 '(mail-user-agent (quote message-user-agent))
 '(make-backup-files nil)
 '(mark-diary-entries-in-calendar t)
 '(mark-holidays-in-calendar t)
 '(max-specpdl-size 1600)
 '(message-log-max 1500)
 '(message-send-mail-partially-limit nil)
 '(mode-line-format (quote (#("-" 0 1 (auto-composed t)) mode-line-mule-info mode-line-modified (-4 . #(" %p" 0 3 (auto-composed t))) (line-number-mode #("[%l,%c]" 0 7 (auto-composed t))) mode-line-buffer-identification #(" %[(" 0 4 (auto-composed t)) mode-name mode-line-process minor-mode-alist "%n" #(")%]--" 0 5 (auto-composed t)) (which-func-mode ("" which-func-format "--")) #("-%-" 0 3 (auto-composed t)))))
 '(mode-require-final-newline nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(newsticker-html-renderer (quote newsticker-htmlr-render))
 '(next-line-add-newlines nil)
 '(parens-require-spaces nil)
 '(pc-selection-mode t)
 '(rcirc-default-nick "AGNUcius")
 '(rcirc-default-server "irc.freenode.net")
 '(rcirc-default-user-full-name "AGNUcius")
 '(rcirc-default-user-name "AGNUcius")
 '(rcirc-startup-channels-alist (quote (("^irc.gnu.org$" "#rcirc"))))
 '(read-quoted-char-radix 10)
 '(scroll-bar-mode (quote left))
 '(scroll-conservatively 50)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72)))
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-debug-buffer t)
 '(undo-limit 800000)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(url-global-history-save-interval 120)
 '(url-keep-history t)
 '(user-full-name "")
 '(user-mail-address "")
 '(vc-default-back-end (quote RCS))
 '(view-read-only t)
 '(visible-bell t)
 '(winner-mode t nil (winner))
 '(woman-use-own-frame nil)
 '(x-stretch-cursor t))




;;;; FILE->MODE ASSOCIATIONS:
(setq auto-mode-alist
      (append
       '(("\\.\\(bas\\|frm\\|dsm\\|ctl\\|cls\\|vb.?\\)\\'" . visual-basic-mode)
         ;; ("\\.cs\\'" . csharp-mode )
         ("\\.cs\\'" . java-mode)
         ("\\.\\(scm\\|smd\\|esh\\|ss\\)\\'" . scheme-mode)
         ("\\.\\(cmd\\|bat\\)\\'" . bat-generic-mode)
         ("\\.css\\'" . css-mode)
         ("\\.cva\\'" . ini-generic-mode)
         ("\\.rc\\'" . rc-generic-mode)
         ("\\.\\(c\\|hp?p?\\)\\'" . c++-mode)
         ("\\.io\\'" . io-mode)
         ("\\.js\\'" . javascript-mode)
         ("\\.java\\'" . java-mode)
         ("\\.lua\\'" . lua-mode)
         ("\\.make?\\'" . makefile-mode)
         ("\\.nsi\\'" . nsi-mode)
         ("\\.php3?\\'" . php-mode)
         ("\\.ps\\'" . ps-mode)
         ("\\.PS1\\'" . powershell-mode)
         ("\\.pyw?\\'" . python-mode)
         ("\\.rb\\'" . ruby-mode)
         ("\\.\\(rar\\|mdz\\)\\'" . archive-mode)
         ("\\.\\([sx]?html?\\|svg\\|rdf\\|sgml\\|wxs\\|wix.*\\|wxi\\|xml\\|xslt?\\|xul\\|aspx?\\|csproj\\|vcproj\\|resx\\)\\'" . nxml-mode)
         ("rfc.*\\'" . rfcview-mode)
         (".sources\\'" . makefile-mode)
         ("\\.cl\\'" . lisp-mode)
         ("/\\.txt/.*" . lens-mode))
       auto-mode-alist))


(run-with-idle-timer
 .1 nil
 (lambda ()
   (message
    (concat "This is "
            (if NoteMacs-NotEmacs
                "NotEmacs"
              "NoteMacs")
            ".  Keys are "
            (if (not NoteMacs-unified-keys) "not ")
            "unified.  "
            "You are "
            (if (not NoteMacs-apprentice) "not ")
            "an apprentice and "
            (if (not NoteMacs-old-timer) "not ")
            "a guru."))))


;;see `list-colors-display'
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.

; '(default ((t (:background "#444" :height 180 :weight bold :family "DejaVu Sans Mono"))))
; '(default ((t (:foreground "#000000" :background "lightskyblue"))))
; '(default ((t (:foreground "color-16" :background "lightskyblue" :height 180 :weight bold :family "DejaVu Sans Mono"))))
 '(default ((t (:foreground "#000" :background "lightskyblue" :height 180 :weight bold :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "red"))))
 '(font-lock-comment-face ((t (:foreground "#333"))))
 '(region ((t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "green")))))




;;; Important keys:

;; C-v  Move forward one screenful
;; M-v  Move backward one screenful

;; C-a  Move to beginning of line
;; C-e  Move to end of line

;; M-<  Move to the beginning of the file
;; M->  Move to the end of the file

;; C-g  keyboard-quit (stop a comman which is taking too long, equivalent
;; to pressing Contol-C in the shell)

;; C-k  kill from the cursor position to end of line
;; C-l  recenter page
;; C-y  runs the command yank
;; M-y  when used after C-y cycles through previous kills

;; C-x u    undo (also C-_ )
;; C-x C-c  Quit Emacs.

;; C-x C-f  Find a file
;; C-x C-q  Toggle `read-only' = enter `view-mode'.
;; C-x C-s  Save the file
;; C-x C-b  List buffers
;; C-x 1    One window (i.e., show only one window)
;; C-x 2    Splits the screen into two windows
;; C-x o    Move the to the 'other' window.
;; C-x b    Switch to buffer (try pressing TAB after C-x b, type the first character of the buffer and then press TAB again)

;; C-s search incremental text through buffer. C-u first for regexp.
;; C-s  search a string (runs isearch-forward)
;; M-%  query replace a string

;; M-q  To re-fill the paragraph

;; M-/ complete word at point
;; C-u `universal' or `prefix' argument adjusts action of next key
;; C-q Quoted-insert = allows you to enter the literal character represented by 'special' keys (for instance C-q C-a would insert )
;; M-; dwim (Do What I Mean) [un]comment region

;; C-x ( start kbd macro
;; C-x ) end kbd macro
;; C-x e call last kbd macro

;; C-h k describe what next press key does
;; C-h c briefly describe what next press key does
;; C-h f describe some function
;; C-h w where is key for command
;; C-h m what do these modes do?
;; C-h b what are the key bindings for these modes?


;;; Some NoteMacs additions:
;; C-R   query and replace regexp through buffer
;; C-(   match parens
;; C-x g goto line
;; M-C-s search recursively in files for regexp. M-, to repeat.
;; M-C-r replace recursively in files. M-, to repeat.
;; M-C-S search recursively for filename. M-, to repeat.

;;; Within `dired':
;; m mark file
;; v  view file (open file in read-only mode)
;; + add directory
;; ! shell command on file


;;; NoteMacs.el ends here
