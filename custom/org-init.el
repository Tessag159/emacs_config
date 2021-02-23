;; Org related configuration

(use-package org
  :mode
  ("\\.org$\\'" . org-mode)
  :interpreter
  ("org" . org-mode)
  :commands
  (org-store-link org-agenda)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :hook
  ((after-init . org-mode)
   (after-init . org-roam-mode)
   (org-mode . visual-line-mode))
  :config
  (setq org-log-done t)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "HOLD" "NEXT" "DONE"))))

(use-package org-ref
  :after
  (org))

(use-package deft
  :after
  (org org-ref))

(use-package org-roam
  :after org
  :config
  (org-roam-mode))

(use-package org-journal
  :config
  (setq org-journal-dir "~/org-journal")
  (setq org-journal-start-on-weekday 7)
  (setq org-journal-date-format "%A, %B %d, %Y")
  (setq org-journal-file-format "%d_%m_%Y")
  :after (org))

(use-package org-bullets
  :hook
  (org-mood . (lambda () (org-bullets-mode 1)))
  :after
  (org))
  
(provide 'org-init)
