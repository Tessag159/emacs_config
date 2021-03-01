;; General configuration

(which-key-mode)
(setq which-key-max-description-length 60)

;; Display line numbers in programming mode
(defun ti/display-line-numbers-on-hook ()
  (display-line-numbers-mode t))

(add-hook 'prog-mode-hook 'ti/display-line-numbers-on-hook)

(use-package emacs
  :init
  ;; Auto-close parens
  (electric-pair-mode +1)
  ;; Disable for <
  (add-function :before-until electric-pair-inhibit-predicate
		(lambda (c) (eq c ?<))))


;; refresh buffer when the file it is visiting changes
(global-auto-revert-mode t)

;; Y or N instead of Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Make .h files c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)

;; Python
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(provide 'general-init)
