;; General settings

(which-key-mode)
(setq which-key-max-description-length 60)

;;Change the size of mark and kill ring
(setq global-mark-ring-max 2500
      mark-ring-max 2500
      kill-ring-max 150
      ;; Stop final newline
      require-final-newline nil
      ;; Stop autosave and backup
      make-backup-files nil
      auto-save-default nil
      ;; Remove scratch message
      initial-scratch-message nil)

;; Display line numbers in programming and org mode
(defun ti/display-line-numbers-on-hook ()
  (display-line-numbers-mode t))


(add-hook 'prog-mode-hook 'ti/display-line-numbers-on-hook)
(add-hook 'org-mode-hook 'ti/display-line-numbers-on-hook)

;; I hate accidentally hitting these
;; Minimize Emacs
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; Capatilize word
(global-unset-key (kbd "M-c"))

;; I never use set-fill-column, and I accidentally
;; hit the binding for it all the time when trying
;; to use helm-find-files
(global-set-key (kbd "C-x f") 'helm-find-files)

;; Allows jumping from window to window
;; by an assigned character
(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)
;; List of characters to assign to windows
(setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

;; Swaps two active windows
(global-set-key (kbd "C-c w") 'ace-swap-window)

;; refresh buffer when the file it is visiting changes
(global-auto-revert-mode t)

;; scroll instead of jump when point leaves top or
;; bottom of buffer
(setq scroll-step 1 scroll-conservatively 10000)
(setq scroll-margin 0)


;; highlighting stuff
(require 'paren)
(require 'rainbow-delimiters)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match "lightcoral")
(set-face-foreground 'show-paren-match "deeppink4")
(rainbow-delimiters-mode)

;; Y or N instead of Yes or No
(fset 'yes-or-no-p 'y-or-n-p)

;; Font face settings
(set-face-attribute 'default nil
		    :family "Hack"
		    :height 108
		    :weight 'normal
		    :width 'normal)

;; Deletes duplicate buffer entries
(setq history-delete-duplicates t)

;; Hippie expansion for word before point
(global-set-key (kbd "M-/") 'hippie-expand)

;; Save bookmark history
(setq bookmark-save-flag t)

;; Make .h files c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
)



;; Python
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Helps me write gooder
(require 'writegood-mode)

;; Check the spelling in the current buffer
(global-set-key (kbd "C-c b") 'ispell-buffer)

;; Provided by https://github.com/ag91
;; and modified slightly
(defun writegood-fk-parameters (&optional rstart rend)
  "Flesch-Kincaid reading parameters"
  (let* ((start (cond (rstart rstart)
                      ((and transient-mark-mode mark-active) (region-beginning))
                      ('t (point-min))))
         (end   (cond (rend rend)
                      ((and transient-mark-mode mark-active) (region-end))
                      ('t (point-max))))
         (words     (float (writegood-count-words start end)))
         (syllables (float (writegood-count-syllables start end)))
         (sentences (float (writegood-count-sentences start end))))
    (list sentences words syllables)))

(defun writegood-reading-ease-score->comment (score) "")

(defun writegood-calculate-reading-ease (&optional start end)
  "Calculate score of Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100."
  (let* ((params (writegood-fk-parameters start end))
	 (sentences (nth 0 params))
	 (words     (nth 1 params))
	 (syllables (nth 2 params))
	 (score  (- 206.835 (* 1.015 (/ words sentences)) (* 84.6 (/ syllables words)))))
    score))

(defun writegood-reading-ease (&optional start end)
  "Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100."
  (interactive)
  (let ((score (writegood-calculate-reading-ease start end)))
    (message "Flesch-Kincaid reading ease score: %.2f %s" score
	     (writegood-reading-ease-score->comment score))))

(defun writegoodmode-reading-ease-thing-at-point (thing)
  (let* ((bounds (bounds-of-thing-at-point thing))
         (b (car bounds))
         (e (cdr bounds)))
    (if (and
         (not (null b))
         (not (null e))
         ;; this is a guess: when the interval between boundaries is
         ;; huge, the paragraph is too big to be validated.
         (< (- e b) 100000))
        (let ((score (writegood-calculate-reading-ease b e)))
          (message "%s reading ease score: %.2f %s" (symbol-name thing) score
		   (writegood-reading-ease-score->comment score))))))

(defun writegoodmode-reading-ease-sentence ()
  (interactive)
  (writegoodmode-reading-ease-thing-at-point 'sentence))

(defun writegoodmode-reading-ease-paragraph ()
  (interactive)
  (writegoodmode-reading-ease-thing-at-point 'paragraph))

(defun writegoodmode-reading-ease-page ()
  (interactive)
  (writegoodmode-reading-ease-thing-at-point 'buffer))(defun writegood-fk-parameters (&optional rstart rend)
  "Flesh-Kincaid reading parameters"
  (let* ((start (cond (rstart rstart)
		      ((and transient-mark-mode mark-active) (region-beginning))
		      ('t (point-min))))
	 (end (cond (rend rend)
		    ((and transient-mark-mode mark-active) (region-end))
		    ('t (point-max))))
	 (words (float (writegood-count-words start end)))
	 (syllables (float (writegood-count-syllables start end)))
	 (sentences (float (writegood-count-sentences start end))))
    (list sentences words syllables)))

(defun writegood-calculate-reading-ease (&optional start end)
  "Calculate score of Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100."
  (let* ((params (writegood-fk-parameters start end))
	 (sentences (nth 0 params))
	 (words     (nth 1 params))
	 (syllables (nth 2 params))
	 (score  (- 206.835 (* 1.015 (/ words sentences)) (* 84.6 (/ syllables words)))))
    score))

;; end provided code

;; Keybindings for the above functions
(global-set-key (kbd "C-c g g") 'writegood-grade-level)
(global-set-key (kbd "C-c g r") 'writegood-reading-ease)
(global-set-key (kbd "C-c g w") 'writegood-mode)
(global-set-key (kbd "C-c v") 'visual-line-mode)


;; Simple scroll from https://github.com/bnbeckwith
;; Modified slightly
(defun ti/scroll-up-1 ()
  "Scroll up by one line."
  (interactive)
  (cua-mode)
  (cua-scroll-up 1)
  (cua-mode))

(defun ti/scroll-down-1 ()
  "Scroll down by one line."
  (interactive)
  (cua-mode)
  (cua-scroll-down 1)
  (cua-mode))

(global-set-key (kbd "M-p") 'ti/scroll-down-1)
(global-set-key (kbd "M-n") 'ti/scroll-up-1)

(defun ti/kill-this-buffer ()
  "Kill the current buffer"
  (interactive)
  (kill-buffer (current-buffer)))


(global-set-key (kbd "C-x C-k") 'ti/kill-this-buffer)

;; End provided code

;; Function from https://github.com/alexkehayias/
;; Rename file and buffer in one shot
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Shortcut for incrementing and decrementing a number under the cursor
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "[-0123456789]+")
      (error "No number at point"))
  ;; This 1+ was a 10+ and I had to modify it back to a 1
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "[-0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;; End provided code

;; Keybindings for above functions
(global-set-key (kbd "C-c f b") 'rename-file-and-buffer)
(global-set-key (kbd "C-c i") 'increment-number-at-point)
(global-set-key (kbd "C-c o") 'decrement-number-at-point)

(provide 'general-init)
