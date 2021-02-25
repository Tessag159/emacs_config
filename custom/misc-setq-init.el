;; Various calls to setq

;; use gdb-many-windows by default
(setq gdb-many-windows t)

;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; Deletes duplicate buffer entries
(setq history-delete-duplicates t)

;; Save bookmark history
(setq bookmark-save-flag t)

;; scroll instead of jump when point leaves top or
;; bottom of buffer
(setq scroll-step 1 scroll-conservatively 10000)
(setq scroll-margin 0)
(setq use-package-compute-statistics t)

;;Change the size of mark and kill ring
(setq global-mark-ring-max 2500)
(setq mark-ring-max 2500)
(setq kill-ring-max 150)

;; Stop final newline
(setq require-final-newline nil)

;; Stop autosave and backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Remove scratch message
(setq initial-scratch-message nil)

(provide 'misc-setq-init)
