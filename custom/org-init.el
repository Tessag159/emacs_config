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
	'((sequence "TODO" "IN-PROGRESS" "HOLD" "NEXT" "Appointment" "|" "CANCELLED" "DONE"))))

(use-package org-ref
  :after
  (org)
  ;; :commands
  ;; ()
  ;; :hook
  ;; ()
  :config
  (setq reftex-default-bibliography '("~/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/bibliography/notes.org")
  (setq org-ref-default-bibliography '("~/bibliography/references.bib"))
  (setq org-ref-pdf-directory "~/bibliography/bibtex-pdfs/")
  (setq bibtex-completion-bibliography "~/bibliography/references.bib")
  (setq bibtex-completion-library-path "~/bibliography/bibtex-pdfs/")
  (setq bibtex-completion-notes-path "~/bibliography/helm-bibtex-notes.org")
  (setq bibtex-completion-pdf-open-function 'org-open-file))

(use-package deft
  :after
  (org org-ref))

(use-package org-roam
  :after org
  :commands
  (org-roam-find-file org-roam-insert org-roam org-ref-helm-insert-cite-link)
  :bind
  ("C-c c f" . org-roam-find-file)
  (:map org-mode-map
	("C-c c i" . org-roam-insert)
	("C-c c b" . org-roam)
	("C-c c c" . org-ref-helm-insert-cite-link))
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  ;; Figure out org-ref to add further information to templates
  (setq org-roam-capture-templates '(("t" "New Thought" plain (function org-roam--capture-get-point)
				      "%?"
				      :file-name "%<%Y%m%d%H%M%S>-${slug}"
				      :head "#+title: ${title}\n#+roam_tags: ${tags}\n"
				      :unnarrowed t)
				     ("a" "Article Reference" plain (function org-roam--capture-get-point)
				      "%?"
				      :file-name "%<%Y%m%d%H%M%S>-${slug}"
				      :head "#+title: ${title}\n#+roam_tags: ${tags}\n#+author: ${author}\n#+type: Article\n#+roam_key: cite:${cite_key}"
				      :unnarrowed t)
				     ("b" "Book Reference" plain (function org-roam--capture-get-point)
				      "%?"
				      :file-name "%<%Y%m%d%H%M%S>-${slug}"
				      :head "#+title: ${title}\n#+roam_tags: ${tags}\n#+author: ${author}\n#+type: Book\n#+roam_key: cite:${cite_key}"
				      :unnarrowed t))))

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
