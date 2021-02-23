;; Doom-modeline configuration

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :init
  (doom-modeline-mode t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :family "Hack" :height 108)
  (set-face-attribute 'mode-line-inactive nil :family "Hack" :height 100))
