(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Refresh package manager archive
(when (not package-archive-contents)
  (package-refresh-contents))

;; Select which packages need to be installed
;; Is this obselete with use-package?
;; Or would it be better to ensure they're all there at startup
;; instead of interrupting to install when I use a package that
;; isn't there?
(setq package-selected-packages '( helm-xref org-ref rainbow-delimiters
					     which-key diff-hl smex git-timemachine
					     deft helm-swoop use-package
					     flycheck org-cliplink org-roam org-download
					     projectile diff-hl org-roam helm-file-preview
					     helm-apt helm-flycheck helm-notmuch notmuch
					     helm-org helm-smex helm-spotify org-books
					     org-autolist org-doing org-msg
					     org-mru-clock org-time-budgets
					     org-tracktable org-wc dashboard python-mode
					     writegood-mode helm-projectile ace-window))



;; Install all packages in the above list if they're not already installed
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Turn on use-package
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Set these early to avoid screen flash
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t
      inhibit-splash-screen t)


;; Would theme theme work better with doom-modeline?
(load-theme 'leuven t)

;; Add custom loaded .el files
(add-to-list 'load-path (expand-file-name "custom/" user-emacs-directory))


;; Load custom .el files
(require 'general-init)
(require 'dashboard-init)
(require 'flycheck-init)
(require 'helm-init)
(require 'org-init)
(require 'projectile-init)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (leuven-theme all-the-icons helm-xref org-ref rainbow-delimiters which-key diff-hl smex git-timemachine deft helm-swoop use-package flycheck org-cliplink org-roam org-download projectile diff-hl org-roam helm-file-preview helm-apt helm-flycheck helm-notmuch notmuch helm-org helm-smex helm-spotify org-books org-autolist org-doing org-msg org-mru-clock org-time-budgets org-tracktable org-wc dashboard python-mode writegood-mode)))
 '(show-paren-mode t)
 '(writegood-weasel-words
   (quote
    ("many" "various" "very" "fairly" "several" "extremely" "exceedingly" "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny" "are a number" "is a number" "excellent" "interestingly" "significantly" "substantially" "clearly" "vast" "relatively" "completely" "literally" "not rocket science" "outside the box" "virtually" "research shows" "studies show" "I would say" "probably" "possibly" "definitely" "could be" "that being said" "most" "a bit" "almost" "basically" "fairly" "often" "usually" "good"))))
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
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-error-face :background "black" :foreground "orange"))))
 '(writegood-duplicates-face ((t (:foreground "#90EE90"))))
 '(writegood-passive-voice-face ((t (:foreground "#FFC1CC"))))
 '(writegood-weasels-face ((t (:foreground "DarkOrange")))))
