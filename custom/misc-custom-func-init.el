;; Custom function initialization

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
(global-set-key (kbd "C-c u") 'decrement-number-at-point)

;; A function that I actually wrote myself, even though I probably didn't need to
(defun ti/indent-buffer ()
  (interactive)
  (push-mark)
  (mark-whole-buffer)
  (crux-cleanup-buffer-or-region)
  (execute-kbd-macro (kbd "C-u C-SPC"))
  (execute-kbd-macro (kbd "C-u C-SPC")))

(global-set-key (kbd "C-c n") 'ti/indent-buffer)


(provide 'misc-custom-func-init)
