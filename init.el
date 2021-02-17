(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-selected-packages '( helm-xref org-ref rainbow-delimiters
					     which-key diff-hl smex git-timemachine
					     helm-flx deft helm-swoop use-package
					     flycheck org-cliplink org-roam org-download
					     projectile diff-hl))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t
      inhibit-splash-screen t)

(set-face-attribute 'default nil
		    :family "Hack"
		    :height 108
		    :weight 'normal
		    :width 'normal)

;; (setq frame-inhibit-implied-resize t)

(add-to-list 'custom-theme-load-path
	     (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'aalto-dark t t)
(enable-theme 'aalto-dark)

(add-to-list 'load-path (expand-file-name "custom/" user-emacs-directory))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Use rainbow-delimiters in Python and CPP
(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)


;; Custom functions provided by willbush
;; https://github.com/willbush

;;;###autoload
(defun my/dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix nil))

;;;###autoload
(defun my/unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos nil))

;;;###autoload
(defun my/kill-all-buffers ()
  "kill all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(require 'general-init)
(require 'flycheck-init)
(require 'helm-init)
(require 'org-init)
(require 'projectile-init)


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
