;; Writegood configuration

;; Provided by https://github.com/ag91
;; and modified slightly

(use-package writegood-mode
  :bind
  ("C-c b" . ispell-buffer)
  ("C-c g g" . writegood-grade-level)
  ("C-c g r" . write-reading-ease)
  ("C-c g w" . writegood-mode)
  ("C-c v" . visual-line-mode)
  :config
  '(((defun writegood-fk-parameters (&optional rstart rend)
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
	(list sentences words syllables)))))
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
       score)))

;; end provided code


(provide 'writegood-init)
