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


;; File creation template
;; Not sure where to put this

;; ("d" "default" plain (function org-roam--capture-get-point)
;;  "%?"
;;  :file-name "%<%d-%m-%y_%H:%M:%S>-${slug}"
;;  :head "#+title: ${title}\n"
;;  :unnarrowed t)


;; I don't want this customization, it just shows proper syntax

;; (setq org-capture-templates
;;       '(    ;; ... other templates
;;         ("j" "Journal Entry"
;;              entry (file+datetree "~/journal.org")
;;              "* %?"
;;              :empty-lines 1)

;;             ;; ... other templates
;;         ))

(provide 'org-init)
