;; General settings

(which-key-mode)

(setq global-mark-ring-max 2500
      mark-ring-max 2500
      require-final-newline nil)

(global-display-line-numbers-mode)


(require 'paren)
(require 'rainbow-delimiters)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match "LightCoral")
(set-face-foreground 'show-paren-match "DeepPink4")

(fset 'yes-or-no-p 'y-or-n-p)


(provide 'general-init)

