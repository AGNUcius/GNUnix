;;; NoteMacs.el --- Make Emacs a little easier to use.

;; Copyright (C): Product Futures Foundation
;; Author: Patrick Anderson <AGNUcius@Gmail.com>

;; This file is Free Software.
;;  You can Use, Modify, Copy and Share it under the
;; terms of the GNU Affero General Public License as
;; published by the Free Software Foundation; either
;; version 3, or (at your option) any later version.

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
;; Emacs cannot see ~/.bashrc settings when launched as X Window app
(setenv "ANDROID_SDK" (concat (getenv "HOME") "/.android/sdk"))
(setenv "ANDROID_HOME" (getenv "ANDROID_SDK"))
(setenv "ANDROID_NDK" (concat (getenv "HOME") "/.android/ndk"))

(setenv "GOPATH" (concat (getenv "HOME") "/go"))

(setenv "PATH" (concat ;;"/usr/local/bin:" ;;for brew on macOS
                       (getenv "HOME") "/bin:" ;;GNUnix stuff
                       (getenv "HOME") "/.cargo/bin:" ;;rust
                       (getenv "GOPATH") "/bin:"
                       (getenv "HOME") "/.npm-global/bin:"
                       (getenv "PATH")))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; (require 'helm)
;; (require 'helm-config)

;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t
;;       helm-echo-input-in-header-line t)

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;       (overlay-put ov 'window (selected-window))
;;       (overlay-put ov 'face
;;                    (let ((bg-color (face-background 'default nil)))
;;                      `(:background ,bg-color :foreground ,bg-color)))
;;       (setq-local cursor-type nil))))


;; (add-hook 'helm-minibuffer-set-up-hook
;;           'spacemacs//helm-hide-minibuffer-maybe)

;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (helm-autoresize-mode 1)

;; (helm-mode 1)


;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; ;; (global-set-key (kbd "C-c g") 'counsel-git)
;; ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; ;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; ;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(setq-default fringes-outside-margins t)
(setq-default left-margin-width 1 right-margin-width 1)

(defalias 'wc 'count-words)
(defalias 'uniq 'delete-duplicate-lines)
(defalias 'omit-lines 'flush-lines)
(defalias 'kill-lines 'flush-lines) ; see 'keep-lines for opposite

;; Change the following values directly by replacing the value 't' with the value 'nil', or vice-versa.  These are not (yet) `defcustom' as maybe they should be.
(defconst NoteMacs-NotEmacs nil "Make Emacs behave like Not Emacs including CUA keys, no infinite undo, and many conflicts with Emacs documentation.")
(defconst NoteMacs-unified-keys t "Make keys more consistient across modes.  Transgresses letter but hopefully not spirit.")
(defconst NoteMacs-apprentice t "Enable medium risk keys and behavior.")
(defconst NoteMacs-old-timer t "Enable advanced keys and behavior.")


;;; Code:
(add-to-list 'load-path "~/NoteMacs/own")
(add-to-list 'load-path "~/NoteMacs/site")
(add-to-list 'load-path "~/NoteMacs/site/git-modes")

;; (ido-mode 1)
;; (ido-everywhere 1)
;; (ido-ubiquitous-mode 1)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(autoload 'magit-status "magit" nil t)

;; (defun magit-diff-master (&optional args)
;;   "Show diff range master...HEAD"
;;   (interactive (list (magit-diff-read-args)))
;;   (magit-diff "master...HEAD" args))

;; (defun magit-diff-mbase (&optional args)
;;   "Show diff of $(git merge-base master HEAD) to working tree."
;;   (interactive (list (magit-diff-read-args)))
;;   (magit-diff-working-tree
;;    (magit-git-string "merge-base" "master" "HEAD") args))

;; (magit-define-popup-action 'magit-diff-popup
;;   ?m "Diff merge-base master" 'magit-diff-mbase)

;; (when (> emacs-major-version 23)
;;   (require 'package)
;;   (package-initialize)
;;   (add-to-list 'package-archives
;; 			   '("melpa" . "http://melpa.milkbox.net/packages/")
;; 			   'APPEND))

;; fix git's weird pager behavior
(setenv "PAGER" "cat")

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


(autoload 'lens-mode "lens" "" t)
(autoload 'lens "lens" "" t)
(autoload 'sic-connect "sic" "simple IMAP client" t)
(autoload 'ewb "ewb" "Emacs Web Browser" t)
(autoload 'hands "hands" "image preview" t)

(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(define-key global-map [(meta control y)] 'browse-kill-ring)
(autoload 'xml-beautify "xml-control" "unwind and indent" t)

(autoload 'findr "findr" "Find file name." t)
(define-key global-map [(meta control S)] 'findr)
;; (autoload 'findr-search "findr" "Find text in files." t)
;; (define-key global-map [(meta control s)] 'findr-search)
(autoload 'findr-query-replace "findr" "Replace text in files." t)
(define-key global-map [(meta control r)] 'findr-query-replace)
;; (define-key global-map [(meta control s)] 'grep-find)
(define-key global-map [(meta control s)] 'ripgrep-regexp)

;; (define-key global-map [(meta control s)]
;;   (lambda (regexp)
;;     (interactive "sregexp: ")
;;     (let (
;;           (buf (get-buffer-create "* ripgrep *")))
;;       (with-current-buffer buf
;;         (grep-mode))
;;       (async-shell-command
;;        (concat "rg --ignore-case --no-heading " regexp) buf))))

;; (define-key global-map [(meta control s)]
;;   (lambda (regexp)
;;     (interactive "sregexp: ")
;;     (let (
;;           (buf (get-buffer-create "* ripgrep *")))
;;       (with-current-buffer buf
;;         (funcall 'grep-mode))
;;       (shell-command
;;        (concat "rg --ignore-case --no-heading " regexp) buf)
;;       (switch-to-buffer buf))))

;; (define-key global-map [(meta control s)]
;;   (lambda (regexp)
;;     (interactive "sregexp: ")
;;     (setq rg-buf-name "*ripgrep*")
;;     (setq rg-buf-buf (get-buffer-create rg-buf-name))
;;     (with-current-buffer rg-buf-buf
;;       (funcall 'grep-mode))
;;     (start-process rg-buf-name rg-buf-buf "rg"
;;                    (concat "--ignore-case --no-heading " regexp))
;;     (switch-to-buffer rg-buf-buf)))

;; (define-key global-map [(meta control s)]
;;   (lambda (regexp)
;;     (interactive "sregexp: ")
;;     (let* (
;;           (name "* ripgrep *")
;;           (buf (get-buffer-create name)))
;;       (set-buffer buf)
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       (insert "-*- mode: grep -*-")
;;       (shell-command
;;        (concat "rg --ignore-case --no-heading " regexp) buf)
;; )))


(autoload 'hexview-find-file "hexview-mode" nil t)
(autoload 'time-insert "time-insert" nil t)
(autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'javascript-mode "javascript" nil t)
(autoload 'tabbar-mode "tabbar" nil t)
(autoload 'powershell-mode "powershell-mode" nil t)
(autoload 'w3m "w3m" nil t)
(autoload 'rfcview-mode "rfcview" "" t)
(autoload 'css-mode "css-mode-min")
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'htmlize-file "htmlize" "" t)
(autoload 'csharp-mode "csharp-mode" "csharp-mode" t)
(autoload 'malyon "malyon" "malyon" t)
(autoload 'ascii-display "ascii" "Toggle on ASCII code display." t)
(autoload 'brightscript-mode "brightscript-mode" "Roku scripting language" t)


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
        ((message "no paren here"))))
(define-key global-map [(control \()] 'match-paren)


(defadvice isearch-exit (after my-goto-match-beginning activate) ;this put the point at the _beginning_ of the match
  "Go to beginning of match."
  (when isearch-forward (goto-char isearch-other-end)))


(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map "\M--"
			  'hs-hide-block)
            (define-key hs-minor-mode-map "\M-="
			  'hs-show-block)
			))

;; from http://www.emacswiki.org/cgi-bin/wiki.pl?EshellFunctions
(add-hook 'eshell-mode-hook
          (lambda ()
			;; (setq eshell-path-env (getenv "PATH"))
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

	  (add-hook 'w3m-mode-hook
				(lambda ()
				  (define-key w3m-mode-map [(u)] 'scroll-down)
				  (define-key w3m-mode-map [(U)] 'w3m-view-parent-page)
				  (define-key w3m-mode-map [(backspace)] 'w3m-view-previous-page)
				  (define-key w3m-mode-map [(meta left)] 'w3m-view-previous-page)
				  (define-key w3m-mode-map [(meta n)] nil) ;unmask my global key
				  (define-key w3m-mode-map [(n)] 'next-line)
				  (define-key w3m-mode-map [(p)] 'previous-line)
				  (define-key w3m-mode-map [(control t)] 'w3m-goto-url-new-session)
				  (define-key w3m-mode-map [(control w)] 'w3m-delete-buffer)
				  (define-key w3m-mode-map [(control o)] 'w3m-goto-url)
				  (define-key w3m-mode-map [(o)] 'w3m-goto-url)
;				  (define-key w3m-mode-map [(control l)] 'w3m-goto-url)
				  (define-key w3m-mode-map [(control k)] 'w3m-search)
				  (define-key w3m-mode-map [(g)] 'w3m-reload-this-page)
				  ))
))


(if NoteMacs-apprentice
    (progn
      (define-key global-map "\M-\r" 'browse-url-at-point)
      ;; (autoload 'skeleton-pair-insert-maybe "skeleton" "" t)
      ;; (setq skeleton-pair t)
      ;; (define-key global-map [(\()] 'skeleton-pair-insert-maybe)
      ;; (define-key global-map [(\[)] 'skeleton-pair-insert-maybe)
      ;; (define-key global-map [({)] 'skeleton-pair-insert-maybe)
      ;; (define-key global-map [(<)] 'skeleton-pair-insert-maybe)
      ;;(define-key global-map [(\")] 'skeleton-pair-insert-maybe)
	  (add-to-list 'same-window-buffer-names "*Buffer List*")
      (define-key Buffer-menu-mode-map " " 'scroll-up)
      (define-key Buffer-menu-mode-map "u" 'scroll-down)
      (define-key Buffer-menu-mode-map "c" 'Buffer-menu-unmark)
	  (define-key Buffer-menu-mode-map "D" '(lambda (&optional arg) (interactive)
                                              (Buffer-menu-delete arg)
                                              (Buffer-menu-switch-other-window)))

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
;;		(lens-follow) ;;go to the most recent month
		(ffap)
        (beginning-of-buffer)
		(toggle-read-only -1)
		(newline 2)
		(forward-line -2)
        (time-insert))

      (add-hook
       'calendar-load-hook
       (lambda ()
         (progn
           (define-key calendar-mode-map "id" ;insert daily
             (lambda nil (interactive)
               (diary-prepend-entry diary-file)))
           (define-key calendar-mode-map "it" ;insert todo
             (lambda nil (interactive)
               (find-file "~/doc/.txt/-todo"))))))

      (add-hook 'ediff-load-hook
                (lambda ()
                  (defun ediff-window-display-p nil)))));force single frame

;; These settings are for those accustomed to Not Emacs behavior,
;; meaning to act even more severly like Windows/CUA and so makes
;; a great solution if you have that finger memory, but conflicts
;; with Emacs keys in major ways.

;; This section is below other NoteMacs levels to ensure priority
;; for key assignments (such as C-z) that are defined in both.
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

;; (require 'powerline)
;; (powerline-default-theme)

;;; Where to put this?
;;these require c-default-style be set to "user"
(c-set-offset 'case-label '+)
(c-set-offset 'inline-open 0)
(c-set-offset 'substatement-open 0)

;; C-TAB cycle through open buffers/windows
;; should use `package-install' here
(if (file-exists-p "~/NoteMacs/site/pc-bufsw.el")
    (progn
      (require 'pc-bufsw)
      (pc-bufsw::bind-keys [C-tab] [C-S-tab]))
  (message "C-Tab buffer switching unavailable"))

;; (global-set-key [(control tab)]       'cycbuf-switch-to-next-buffer)
;; (global-set-key [(shift control tab)]        'cycbuf-switch-to-previous-buffer)


;;; Customized variables
;;  press f1 v RET  while over any variable name
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-scroll-prefer-subnodes nil)
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
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
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
 '(diary-file "~/doc/.txt/-diary")
 '(diary-hook (quote (appt-make-list)))
 '(dired-at-point-require-prefix t)
 '(dired-listing-switches "-alh")
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
 '(garbage-collection-messages t)
 '(gdb-show-main t)
 '(generic-define-unix-modes t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t nil (hl-line))
 '(grep-command "grep -inH -e ")
 '(grep-find-command (quote ("find . -type f -exec grep -inH -e  {} +" . 35)))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -inH -e <R> {} +")
 '(grep-template "grep <X> <C> -inH -e <R> <F>")
 '(gud-cdb-directories (quote (".\\" "..\\")))
 '(gud-chdir-before-run nil)
 '(helm-youtube-key (quote AIzaSyCYWv1p8mX8QX0eac0QZfQ7GjXPztvJW1A))
 '(hi-lock-mode t t (hi-lock))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-from-kill try-expand-dabbrev-all-buffers try-complete-file-name try-expand-list try-expand-line try-complete-lisp-symbol)))
 '(hl-line-face (quote trailing-whitespace))
 '(hs-isearch-open t)
 '(ibuffer-expert nil)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-use-other-window t)
 '(indent-tabs-mode nil)
 '(iswitchb-mode t)
 '(large-file-warning-threshold nil)
 '(lazy-highlight-cleanup t)
 '(lazy-highlight-initial-delay 0)
 '(lazy-highlight-max-at-a-time nil)
 '(line-move-visual nil)
 '(list-command-history-max 128)
 '(ls-lisp-verbosity nil)
 '(lua-comment-start "--")
 '(lua-indent-level 4)
 '(magit-diff-arguments (quote ("--ignore-all-space" "--no-ext-diff" "--stat")))
 '(mail-self-blind t)
 '(mail-user-agent (quote message-user-agent))
 '(make-backup-files nil)
 '(mark-diary-entries-in-calendar t)
 '(mark-holidays-in-calendar t)
 '(max-specpdl-size 1600)
 '(message-log-max 1500)
 '(message-send-mail-partially-limit nil)
 '(mode-require-final-newline nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(newsticker-html-renderer (quote newsticker-htmlr-render))
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (charmap math-symbol-lists wanderlust markdown-mode rainbow-delimiters ripgrep nim-mode hide-lines flycheck-nimsuggest flycheck-rust flycheck-nim cycbuf ace-isearch magit pdf-tools lua-mode helm-youtube google-this bm)))
 '(parens-require-spaces nil)
 '(pc-selection-mode t)
 '(read-buffer-completion-ignore-case t)
 '(read-quoted-char-radix 10)
 '(ripgrep-arguments (quote ("-i")))
 '(scroll-bar-mode (quote left))
 '(scroll-conservatively 50)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72)))
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-debug-buffer t)
 '(undo-limit 800000)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(url-global-history-save-interval 120)
 '(url-keep-history t)
 '(vc-default-back-end (quote RCS))
 '(vc-git-annotate-switches "-w")
 '(view-read-only t)
 '(w3m-session-crash-recovery nil)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))))
 '(winner-mode t nil (winner))
 '(woman-use-own-frame nil)
 '(x-stretch-cursor t))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;;; FILE->MODE ASSOCIATIONS:
(setq auto-mode-alist
      (append
       '(
         ("\\.brs$" . brightscript-mode)
         ("\\.\\(bas\\|frm\\|dsm\\|ctl\\|cls\\|vb.?\\)\\'" . visual-basic-mode)
         ;; ("\\.cs\\'" . csharp-mode )
         ("\\.cs\\'" . java-mode)
         ("\\.\\(scm\\|smd\\|esh\\|ss\\)\\'" . scheme-mode)
         ("\\.css\\'" . css-mode)
         ("\\.cva\\'" . ini-generic-mode)
         ("\\.rc\\'" . rc-generic-mode)
         ("\\.\\(c\\|hp?p?\\)\\'" . c++-mode)
         ("\\.io\\'" . io-mode)
         ("\\.js\\'" . javascript-mode)
         ("\\.java\\'" . java-mode)
         ("\\.lua\\'" . lua-mode)
         ("\\.make?\\'" . makefile-mode)
		 ("\\.mm\\'" . objc-mode)
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


;;try  M-x  list-colors-display
;; calm
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :background "#505050" :foreground "#000000" :height 240))))
;;  '(cursor ((t (:background "green"))))
;;  '(font-lock-comment-face ((t (:foreground "#202020"))))
;;  '(font-lock-keyword-face ((t (:foreground "#000080" :background "#494950"))))
;;  '(region ((t (:inverse-video t))))
;;  '(trailing-whitespace ((t (:background "#606060"))))
;;  '(w3m-anchor ((t (:inherit font-lock-keyword-face)))))

;; ;;daylight
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "skyblue" :foreground "#000000" :height 240))))
 '(cursor ((t (:background "green"))))
 '(font-lock-comment-face ((t (:foreground "#808080"))))
 '(font-lock-keyword-face ((t (:foreground "#808080"))))
 '(region ((t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "#eeeeee"))))
 '(w3m-anchor ((t (:inherit font-lock-keyword-face)))))

;; ;darker
;; (custom-set-faces
;;  '(default ((t (:foreground "gray0" :background "#181818" :height 165 :weight bold))))
;;  '(cursor ((t (:background "magenta"))))
;;  '(font-lock-comment-face ((t (:foreground "gray25"))))
;;  '(font-lock-keyword-face ((t (:foreground "#000080" :background "#232318"))))
;;  '(region ((t (:inverse-video t))))
;;  '(trailing-whitespace ((t (:background "gray45"))))
;;  '(w3m-anchor ((t (:inherit font-lock-keyword-face)))))

;; (defun csf (bg cur com)
;;   (custom-set-faces
;;    '(default ((t (:foreground "gray0" :background bg :height 180 :weight bold))))
;;    '(cursor ((t (:background cur))))
;;    '(font-lock-comment-face ((t (:foreground com))))))

;; aqua
;; (csf "#005959" "yellow" "#590000")

;; olive
;; (custom-set-faces
;;  '(default ((t (:foreground "gray0" :background "#595900" :height 180 :weight bold))))
;;  '(cursor ((t (:background "green"))))
;;  '(font-lock-comment-face ((t (:foreground "#000059")))))

;; grape
;; (custom-set-faces
;;  '(default ((t (:foreground "gray0" :background "#590059" :height 180 :weight bold))))
;;  '(cursor ((t (:background "red"))))
;;  '(font-lock-comment-face ((t (:foreground "#005900")))))

;;; sage
;; (custom-set-faces
;;  '(default ((t (:foreground "gray0" :background "#599959" :height 180 :weight bold))))
;;  '(cursor ((t (:background "blue"))))
;;  '(font-lock-comment-face ((t (:foreground "#ff99ff")))))


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
