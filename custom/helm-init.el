;; Helm configuration

;; Turns on helm

(use-package helm
  :bind
  (([remap find-file] . helm-find-files)
	 ([remap execute-extended-command] . helm-M-x)
	 ([remap switch-to-buffer] . helm-mini)
	 ([remap list-buffers] . helm-mini)

	 ("M-y" . helm-show-kill-ring)
	 ("C-c s" . helm-multi-swoop-all)
	 ("C-c r" . recentf)
	 ("C-h SPC" . helm-mark-ring)
	 ("C-h C-SPC" . helm-all-mark-rings)
	 ("C-c h o" . helm-occur)
	 ("C-c h g" . helm-google-suggest))
  :config
  (helm-mode)
  ;; Stop helm from popping up in a separate window if one is open
  (setq helm-split-window-in-side-p t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-split-window-function 'helm-default-display-buffer)
  (setq helm-swoop-split-with-multiple-windows nil)
  ;;Allow typing in helm header instead of minibuffer
  (setq helm-echo-input-in-header-line t)
  (defvar bottom-buffers nil
  "List of bottom buffers before helm session.
    Its element is a pair of `buffer-name' and `mode-line-format'.")

  (defun bottom-buffers-init ()
    (setq-local mode-line-format (default-value 'mode-line-format))
    (setq bottom-buffers
	  (cl-loop for w in (window-list)
		   when (window-at-side-p w 'bottom)
		   collect (with-current-buffer (window-buffer w)
			     (cons (buffer-name) mode-line-format)))))

  (defun bottom-buffers-hide-mode-line ()
    (setq-default cursor-in-non-selected-windows nil)
    (mapc (lambda (elt)
	    (with-current-buffer (car elt)
	      (setq-local mode-line-format nil)))
	  bottom-buffers))

  (defun bottom-buffers-show-mode-line ()
    (setq-default cursor-in-non-selected-windows t)
    (when bottom-buffers
      (mapc (lambda (elt)
	      (with-current-buffer (car elt)
		(setq-local mode-line-format (cdr elt))))
	    bottom-buffers)
      (setq bottom-buffers nil)))

  (defun helm-keyboard-quit-advice (orig-func &rest args)
    (bottom-buffers-show-mode-line)
    (apply orig-func args))

  ;; Hide minibuffer while helm is active
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	(overlay-put ov 'window (selected-window))
	(overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
				`(:background ,bg-color :foreground ,bg-color)))
	(setq-local cursor-type nil))))
  (advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)
  :hook
  (helm-before-initialize . bottom-buffers-init)
  (helm-after-initialize . bottom-buffers-hide-mode-line)
  (helm-exit-minibuffer-hook . bottom-buffers-show-mode-line)
  (helm-cleanup-hook . bottom-buffers-show-mode-line)
  (helm-minibuffer-set-up-hook . helm-hide-minibuffer-maybe))


(provide 'helm-init)
