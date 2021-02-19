;; Org configuration


(require 'org)
(require 'deft)
(require 'org-ref)
(require 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-log-done t)

(setq org-roam-directory "/home/tess/org-roam")

(add-hook 'after-init-hook 'org-roam-mode)



;; File creation template
;; Not sure where to put this

;; ("d" "default" plain (function org-roam--capture-get-point)
;;  "%?"
;;  :file-name "%<%d-%m-%y_%H:%M:%S>-${slug}"
;;  :head "#+title: ${title}\n"
;;  :unnarrowed t)


;; I don't want this, it just shows syntax

;; (setq org-capture-templates
;;       '(    ;; ... other templates

;;         ("j" "Journal Entry"
;;              entry (file+datetree "~/journal.org")
;;              "* %?"
;;              :empty-lines 1)

;;             ;; ... other templates
;;         ))

(provide 'org-init)
