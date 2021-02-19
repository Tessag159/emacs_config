;; Helm configuration

;; Turns on helm
(require 'helm-config)
(helm-mode)

;; Stop helm from popping up in a separate window if one is open
(setq helm-split-window-in-side-p t)

;; These lines break everything by not allowing me to use
;; C-h m when the helm buffer is up

;;(add-to-list 'display-buffer-alist
;;             '("\\`\\*helm.*\\*\\'"
;;               (display-buffer-in-side-window)
;;               (inhibit-same-window . t)
;;               (window-height . 0.4)))

(setq helm-swoop-split-with-multiple-windows nil
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-split-window-function 'helm-default-display-buffer)

;;Allow typing in helm buffer instead of minibuffer
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


(add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
(add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
(add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
(add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
(advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

;; Hide minibuffer while helm is active
(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)


;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
; (helm-mode)
; (require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(define-key global-map [remap list-buffers] #'helm-mini)


(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)
(global-set-key (kbd "C-c r") 'helm-recentf)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(setq helm-swoop-split-direction 'split-window-vertically)


(provide 'helm-init)
