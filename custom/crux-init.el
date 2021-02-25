;; Crux configuration


(use-package crux
  :bind
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c ," . crux-find-user-init-file)
  ([remap open-line] . crux-smart-open-line)
  ([remap move-beginning-of-line] . crux-move-beginning-of-line))

(provide 'crux-init)
