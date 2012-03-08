;;; gen-sensible-defaults.el --- generate sensible defaults for .emacs

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

;;

;;; Code:

(defgroup gen-sensible-defaults nil
  "sensible defaults for .emacs"
  :group 'emacs)

(mapcar (lambda (var)
		  (custom-add-to-group 'gen-sensible-defaults var 'custom-variable))
		'(
		  ange-ftp-dumb-unix-host-regexp
		  ange-ftp-generate-anonymous-password
		  appt-message-warning-time
		  apropos-do-all
		  archive-zip-use-pkzip
		  auto-compression-mode
		  auto-save-list-file-prefix
		  blink-cursor
		  blink-cursor-delay
		  blink-cursor-interval
		  blink-matching-paren-dont-ignore-comments
		  bookmark-bmenu-toggle-filenames
		  bookmark-sort-flag
		  browse-url-browser-function
		  c-default-style
		  c-echo-syntactic-information-p
		  c-mode-common-hook
		  case-fold-search
		  column-number-mode
		  completion-ignored-extensions
		  confirm-kill-emacs
		  delete-selection-mode
		  desktop-enable
		  diary-file
		  diary-hook
		  dired-at-point-require-prefix
		  dired-recursive-copies
		  dired-recursive-deletes
		  display-time-day-and-date
		  display-time-format
		  display-time-mode
		  ediff-diff-options
		  ediff-window-setup-function
		  emacs-lisp-mode-hook
		  emacs-wiki-downcase-title-words
		  emacs-wiki-highlight-markup
		  enable-recursive-minibuffers
		  eol-mnemonic-mac
		  eol-mnemonic-undecided
		  eol-mnemonic-unix
		  eshell-buffer-maximum-lines
		  eshell-cmpl-man-function
		  eshell-directory-name
		  eshell-force-execution
		  eshell-group-file
		  eshell-hosts-file
		  eshell-interpreter-alist
		  eshell-ls-archive-regexp
		  eshell-ls-dired-initial-args
		  eshell-ls-highlight-alist
		  eshell-ls-initial-args
		  eshell-ls-product-regexp
		  eshell-ls-use-in-dired
		  eshell-passwd-file
		  eshell-send-direct-to-subprocesses
		  flyspell-default-dictionary
		  font-lock-maximum-size
		  generic-define-unix-modes
		  global-font-lock-mode
		  global-hl-line-mode
		  gnus-asynchronous
		  gnus-default-directory
		  gnus-group-list-inactive-groups
		  gnus-home-directory
		  gnus-large-newsgroup
		  gnus-message-archive-group
		  gnus-use-cache
		  gnus-use-dribble-file
		  gud-cdb-directories
		  gud-chdir-before-run
		  hi-lock-mode
		  highlight-changes-active-string
		  highlight-changes-global-changes-existing-buffers
		  highlight-changes-passive-string
		  hippie-expand-try-functions-list
		  hs-isearch-open
		  htmlize-html-major-mode
		  isearch-lazy-highlight-cleanup
		  isearch-lazy-highlight-initial-delay
		  isearch-lazy-highlight-max-at-a-time
		  iswitchb-mode
		  ls-lisp-emulation
		  ls-lisp-ignore-case
		  ls-lisp-verbosity
		  mail-host-address
		  mail-source-crash-box
		  mail-source-directory
		  mail-user-agent
		  make-backup-files
		  mark-diary-entries-in-calendar
		  mark-holidays-in-calendar
		  max-specpdl-size
		  message-directory
		  message-log-max
		  message-send-mail-function
		  mmm-global-mode
		  mmm-major-mode-preferences
		  mmm-mode-ext-classes-alist
		  mode-line-format
		  mouse-wheel-mode
		  next-line-add-newlines
		  next-screen-context-lines
		  nnmail-message-id-cache-file
		  parens-require-spaces
		  read-quoted-char-radix
		  resize-minibuffer-mode
		  scroll-bar-mode
		  scroll-preserve-screen-position
		  send-mail-function
		  shell-file-name
		  show-paren-mode
		  show-trailing-whitespace
		  smtpmail-auth-credentials
		  smtpmail-debug-info
		  smtpmail-default-smtp-server
		  smtpmail-local-domain
		  smtpmail-queue-dir
		  smtpmail-smtp-server
		  suggest-key-bindings
		  tab-stop-list
		  tab-width
		  tags-revert-without-query
		  tramp-debug-buffer
		  transient-mark-mode
		  uniquify-buffer-name-style
		  url-cache-directory
		  url-cookie-file
		  url-global-history-file
		  url-global-history-save-interval
		  url-keep-history
		  user-full-name
		  user-mail-address
		  vc-default-back-end
		  view-diary-entries-initially
		  w3m-bookmark-file
		  w3m-default-save-directory
		  w3m-fill-column
		  w3m-key-binding
		  w3m-local-directory-view-method
		  w3m-search-default-engine
		  w3m-use-header-line
		  winner-mode
		  woman-use-own-frame
		  x-stretch-cursor
		  ))

(customize-group 'gen-sensible-defaults)

  (provide 'gen-sensible-defaults)
;;; gen-sensible-defaults.el ends here
