
(require 'dashboard)

(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome back Tess")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-items '((recents . 12)
		  (projects . 5)
		  (agenda  . 5)))

(provide 'dashboard-init)
