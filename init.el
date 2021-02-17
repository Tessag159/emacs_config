(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-selected-packages '( helm-xref org-ref rainbow-delimiters
					     which-key diff-hl smex git-timemachine
					     helm-flx deft helm-swoop use-package
					     flycheck))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
 (mapc #'package-install package-selected-packages))

(which-key-mode)



;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;; Editor ;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

(setq global-mark-ring-max 2500
      mark-ring-max 2500
      mode-require-final-newline t)

(global-display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;; General ;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t
      inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(require 'paren)
(require 'rainbow-delimiters)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match "LightCoral")
(set-face-foreground 'show-paren-match "DeepPink4")


(add-to-list 'custom-theme-load-path
	     (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'aalto-dark t t)
(enable-theme 'aalto-dark)


(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;; Helm ;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

;; Turns on helm
(require 'helm-config)
(helm-mode 1)
(helm-flx-mode 1)

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
(helm-mode)
(require 'helm-xref)
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
(set-face-attribute 'helm-selection nil
		    :background "midnight blue")


;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;; Org ;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;


(require 'org)
(require 'deft)
(require 'org-ref)
(require 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c C-t") 'org-todo)
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-log-done t)


;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;; Flycheck ;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;


(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-include-path
			   (list (expand-file-name "/usr/local/include/opencv4")))))
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++14")))

(add-hook 'python-mode-hook 'flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (magit magit-todos org-ref helm-git smex helm-flx helm-swoop rainbow-delimiters yasnippet avy which-key helm-xref)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 108 :width normal))))
 '(rainbow-delimiters-base-error-face ((t nil)))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#e40000"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff7400"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#e1ff00"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#1cff00"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#00efff"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#1700ff"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ac00ff"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff00f8"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff006e"))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face :background "black" :foreground "green"))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-error-face :background "black" :foreground "orange")))))
