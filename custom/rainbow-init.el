;; Rainbow configuration

;; highlighting stuff
(require 'paren)
(require 'rainbow-delimiters)
(show-paren-mode 1)
(setq show-paren-delay 0)
;; Hooks just don't work for this program
;; (add-hook 'prog-mode #'rainbow-delimiters-mode)

;; Take from zone.el and modified to rainbow instead
(defvar rainbow-timer nil)
(defun rainbow-when-idle (secs)
  "Rainbow out when Emacs has been idle for SECS seconds."
  (interactive "nHow long before I start rainbowing? (seconds): ")
  (if (timerp rainbow-timer)
      (cancel-timer rainbow-timer))
  (setq rainbow-timer nil)
  (or (<= secs 0)
      (setq rainbow-timer (run-with-idle-timer secs t 'zone-rainbow))))
(defun rainbow-leave-me-alone ()
  "Don't zone out when Emacs is idle."
  (interactive)
  (if (timerp rainbow-timer)
      (cancel-timer rainbow-timer))
  (setq rainbow-timer nil)
  (message "I won't zone out any more"))

(defun ti/rainbow-stuff ()
  "Used to turn on various rainbow-y packages like rainbow-delimiters
zone-rainbow and rainbow-mode"
  (rainbow-delimiters-mode)
  (rainbow-mode))

(add-hook 'prog-mode-hook 'ti/rainbow-stuff)

(provide 'rainbow-init)
