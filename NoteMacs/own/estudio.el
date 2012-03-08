;;; estudio.el --- control DevStudio using Windows Scripting Host (wsh) [cscript.exe or wscript.exe]

;; Author: Patrick Anderson 
;; Home:   http://www.hobbiton.org/~adbuster/emacs
;;     soon at: http://sourceforge.net/projects/estudio
;; Version: 1

;require
;http://www.atnetsend.net/computing/VisEmacs/

;install
;this file in your load path
;the .vbs files (in the own-lisp/estudio dir) in your .exe path.

;add
; (require 'estudio)
;to your .emacs file

;execute
; M-x eval-buffer
;so you don't have to restart

;todo:
;if more than one DevStudio is open, you cannot specify which to control


(require 'cc-mode)
(define-key c++-mode-map [(f1)] (lambda () (interactive) (w32-shell-execute "open" "HelpSystemIndex.vbs")))
(define-key c++-mode-map [(f4)] (lambda () (interactive) (w32-shell-execute "open" "GoToNextErrorTag.vbs")))
(define-key c++-mode-map [(shift f4)] (lambda () (interactive) (w32-shell-execute "open" "GoToPrevErrorTag.vbs")))
(define-key c++-mode-map [(f5)] (lambda () (interactive) (w32-shell-execute "open" "DebugGo.vbs")))
(define-key c++-mode-map [(shift f5)] (lambda () (interactive) (w32-shell-execute "open" "DebugStopDebugging.vbs")))
;(define-key c++-mode-map [(f7)] (lambda () (interactive) (w32-shell-execute "open" "Build.vbs")))
(define-key c++-mode-map [(M-f7)] (lambda () (interactive) (w32-shell-execute "open" "ProjectSettings.vbs")))
(define-key c++-mode-map [(f9)] (lambda () (interactive) (w32-shell-execute "open" "DebugToggleBreakpoint.vbs")))
(define-key c++-mode-map [(f10)] (lambda () (interactive) (w32-shell-execute "open" "DebugStepOver.vbs")))
(define-key c++-mode-map [(f11)] (lambda () (interactive) (w32-shell-execute "open" "DebugStepInto.vbs")))
(define-key c++-mode-map [(shift f11)] (lambda () (interactive) (w32-shell-execute "open" "DebugStepOut.vbs")))

;(define-key c++-mode-map [(f12)] (lambda () (interactive) (w32-shell-execute "open" "BrowseGoToDefinition.vbs")))
(define-key c++-mode-map [(pause)] (lambda () (interactive) (w32-shell-execute "open" "DebugBreak.vbs")))
(define-key c++-mode-map [(control pause)] (lambda () (interactive) (w32-shell-execute "open" "BuildStop.vbs")))

(provide 'estudio)
