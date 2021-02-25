;; Ace-window configuration

;; Package for window management
(use-package ace-window
  :bind
  ("C-c o" . ace-window)
  ("C-c w" . ace-swap-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

(provide 'ace-window-init)
