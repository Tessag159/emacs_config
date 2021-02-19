;; Org-journal configuration

(require 'org-journal)

;; Journals will be split daily
(setq org-journal-file-type 'daily)
(setq org-journal-dir (list(expand-file-name "~/org-journal/")))


(provide 'org-journal-init)
