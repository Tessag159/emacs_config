;; Org (and org-roam, for some reason) configuration

(use-package org
  :mode ("\\.org$\\'" . org-mode)
  :interpreter ("org" . org-mode)
  :commands (org-store-link org-agenda)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :hook
  ((after-init . org-mode)
   (after-init . org-roam-mode))
  :config
  (setq org-log-done t))

(use-package org-ref
  :after (org))

(use-package deft
  :after(org org-ref))

(use-package org-roam
  :after org
  :config (org-roam-mode))

(use-package org-journal
  :config
  (setq org-journal-dir "~/org-journal")
  (setq org-journal-start-on-weekday 7)
  (setq org-journal-date-format "%A, %B %d, %Y")
  (setq org-journal-file-format "%d_%m_%Y")
  :after (org))

;; (setq org-capture-templates
;; 	'(("d" "default" plan (function org-journal-find-location)
;; 	   "%?"
;; 	   :file-name "%<%A-%N-%d-%Y_%H:%M:%S>-${slug}"
;; 	   :head "#+title: ${title}\n"
;; 	   :unnarrowed t)))

(provide 'org-init)
