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
   (org-mode . visual-line-mode)
   (org-mode . variable-pitch-mode))
  :config
  (setq org-log-done t)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "HOLD" "NEXT" "|" "CANCELLED" "DONE"))))

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
  ;; This stops org-journal from loading for some reason...
  ;; :bind
  ;; (("C-c f r" . org-remove-file)
  ;;  ("C-c f a" . org-agenda-file-to-front))
  :config
  (setq org-journal-dir "~/org-journal")
  (setq org-journal-start-on-weekday 7)
  (setq org-journal-date-format "%A, %B %d, %Y")
  (setq org-journal-file-format "%d_%m_%Y")
  (global-set-key (kbd "C-c f r") 'org-remove-file)
  (global-set-key (kbd "C-c f a") 'org-agenda-file-to-front)
  :after (org))

;; Pretty stuff
;; From https://mstempl.netlify.app/post/beautify-org-mode/

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq org-startup-indented t)
(setq org-src-tab-acts-natively t)
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)

;; End provided code

(provide 'org-init)
