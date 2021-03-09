;; Org related configuration

(use-package org
  :mode
  ("\\.org$\\'" . org-mode)
  :interpreter
  ("org" . org-mode)
  :commands
  (org-store-link org-agenda org-remove-file
		  org-agenda-file-to-front org-goto)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c f r" . org-remove-file)
  ("C-c f a" . org-agenda-file-to-front)
  ("C-c j" . org-goto)
  :hook
  ((after-init . org-mode)
   (after-init . org-roam-mode)
   (org-mode . visual-line-mode)
   (org-mode . variable-pitch-mode))
  :config
  (setq org-log-done t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "HOLD" "NEXT" "APPOINTMENT" "|" "CANCELED" "DONE"))))

(defvar books-file "/home/tess/org/Books.org"
  "The absolute value of the file where I keep my books stored on my system.")

(defun ti/open-books-file ()
  "Opens the file located at 'books file' if the variable exists."
  (interactive)
  (if books-file
      (find-file books-file)
    (message "No books file provided.")))

;; Provided by https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification/16746#16746
;; Modified to have my initials to remain consistent with my other function definitions
(defun ti/org-entity-get-name (char)
  "Return the entity name for CHAR. For example, return \"ast\" for *."
  (let ((ll (append org-entities-user
		    org-entities))
	e name utf8)
    (catch 'break
      (while ll
	(setq e (pop ll))
	(when (not (stringp e))
	  (setq utf8 (nth 6 e))
	  (when (string= char utf8)
	    (setq name (car e))
	    (throw 'break name)))))))



(defun ti/org-insert-org-entity-maybe (&rest args)
  "When the universal prefix C-u is used before entering any character,
    insert the character's `org-entity' name if available.

    If C-u prefix is not used and if `org-entity' name is not available, the
    returned value `entity-name' will be nil."
  ;; It would be fine to use just (this-command-keys) instead of
  ;; (substring (this-command-keys) -1) below in emacs 25+.
  ;; But if the user pressed "C-u *", then
  ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
  ;;  - in emacs 25.x, (this-command-keys) would return "*".
  ;; But in both versions, (substring (this-command-keys) -1) will return
  ;; "*", which is what we want.
  ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
  (let ((pressed-key (substring (this-command-keys) -1))
	entity-name)
    (when (and (listp args) (eq 4 (car args)))
      (setq entity-name (ti/org-entity-get-name pressed-key))
      (when entity-name
	(setq entity-name (concat "\\" entity-name "{}"))
	(insert entity-name)
	(message (concat "Inserted `org-entity' "
			 (propertize entity-name
				     'face 'font-lock-function-name-face)
			 " for the symbol "
			 (propertize pressed-key
				     'face 'font-lock-function-name-face)
			 "."))))
    entity-name))

;; Run `org-self-insert-command' only if `ti/org-insert-org-entity-maybe'
;; returns nil.
(advice-add 'org-self-insert-command :before-until #'ti/org-insert-org-entity-maybe)

;; End provided code

;; (use-package org-ref
;;   :after
;;   (org)
;;   :commands
;;   ()
;;   :bind
;;   ()
;;   :config
;;   (setq reftex-default-bibliography '("~/bibliography/references.bib"))
;;   (setq org-ref-bibliography-notes "~/bibliography/notes.org")
;;   (setq org-ref-default-bibliography '("~/bibliography/references.bib"))
;;   (setq org-ref-pdf-directory "~/bibliography/bibtex-pdfs/")
;;   (setq bibtex-completion-bibliography "~/bibliography/references.bib")
;;   (setq bibtex-completion-library-path "~/bibliography/bibtex-pdfs/")
;;   (setq bibtex-completion-notes-path "~/bibliography/helm-bibtex-notes.org")
;;   (setq bibtex-completion-pdf-open-function 'org-open-file))


(use-package org-roam
  :after org
  :commands
  (org-roam-find-file org-roam-insert org-roam org-ref-helm-insert-cite-link)
  :bind
  ("C-c c f" . org-roam-find-file)
  ("C-c C-b" . ti/open-books-file)
  (:map org-mode-map
	("C-c c i" . org-roam-insert)
	("C-c c b" . org-roam))
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
  :config
  (setq org-journal-dir "~/org-journal")
  (setq org-journal-start-on-weekday 7)
  (setq org-journal-date-format "%A, %B %d, %Y")
  (setq org-journal-file-format "%d_%m_%Y")
  :after (org))

;; Pretty stuff
;; From https://mstempl.netlify.app/post/beautify-org-mode/

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq org-src-tab-acts-natively t)
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)

;; End provided code

(provide 'org-init)
