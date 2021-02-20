;; Projectile settings

(use-package projectile
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config (projectile-mode 1))


(provide 'projectile-init)
