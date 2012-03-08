(defun ansynch-fontify-after-defer ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all windows where deferral has occurred for its buffer.
  (save-excursion
    (while (and ansynch-buffers (not (input-pending-p)))
      (let ((buffer (car ansynch-buffers)) windows)
		;; Paranoia: check that the buffer is still live and Lazy Lock mode on.
		(when (buffer-live-p buffer)
		  (set-buffer buffer)
		  (when ansynch-mode
			(setq windows (get-buffer-window-list buffer 'nomini t))
			(while windows
			  (ansynch-fontify-window (car windows))
			  (setq windows (cdr windows)))))
		(setq ansynch-buffers (cdr ansynch-buffers))))))


(defun ansynch-fontify-after-idle ()
  ;; Called from `timer-idle-list'.
  ;; Fontify all buffers that need it, stealthily while idle.
  (unless (or executing-kbd-macro (window-minibuffer-p (selected-window)))
    ;; Loop over all buffers, fontify stealthily for each if necessary.
    (let ((buffers (buffer-list)) (continue t)
		  message message-log-max minibuffer-auto-raise)
      (save-excursion
		(do-while (and buffers continue)
				  (set-buffer (car buffers))
				  (if (not (and ansynch-mode (ansynch-unfontified-p)))
					  (setq continue (not (input-pending-p)))
					;; Fontify regions in this buffer while there is no input.
					(with-temp-message
						(when ansynch-stealth-verbose
						  "Fontifying stealthily...")
					  (do-while (and (ansynch-unfontified-p) continue)
								(if (and ansynch-stealth-load
										 (> (car (load-average)) ansynch-stealth-load))
									;; Wait a while before continuing with the loop.
									(progn
									  (when message
										(message "Fontifying stealthily...suspended")
										(setq message nil))
									  (setq continue (sit-for (or ansynch-stealth-time 30))))
								  ;; Fontify a chunk.
								  (when ansynch-stealth-verbose
									(if message
										(message "Fontifying stealthily... %2d%% of %s"
												 (ansynch-percent-fontified) (buffer-name))
									  (message "Fontifying stealthily...")
									  (setq message t)))
								  (ansynch-fontify-chunk)
								  (setq continue (sit-for (or ansynch-stealth-nice 0)))))))
				  (setq buffers (cdr buffers)))))))

(defun ansynch-install-timers (dtime stime)
  ;; Schedule or re-schedule the deferral and stealth timers.
  ;; The layout of `ansynch-timers' is:
  ;;  ((DEFER-TIME . DEFER-TIMER) (STEALTH-TIME . STEALTH-TIMER)
  ;; If an idle timeout has changed, cancel the existing idle timer (if there
  ;; is one) and schedule a new one (if the new idle timeout is non-nil).
  (unless (eq dtime (car (car ansynch-timers)))
    (let ((defer (car ansynch-timers)))
      (when (cdr defer)
		(cancel-timer (cdr defer)))
      (setcar ansynch-timers (cons dtime (and dtime
											  (run-with-idle-timer dtime t 'ansynch-fontify-after-defer))))))
  (unless (eq stime (car (cdr ansynch-timers)))
    (let ((stealth (cdr ansynch-timers)))
      (when (cdr stealth)
		(cancel-timer (cdr stealth)))
      (setcdr ansynch-timers (cons stime (and stime
											  (run-with-idle-timer stime t 'ansynch-fontify-after-idle)))))))
