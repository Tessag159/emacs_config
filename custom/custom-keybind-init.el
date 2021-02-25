;; Custom keybindings for various built-in emacs things

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

;; Calendar access
(global-set-key (kbd "M-c") 'calendar)

;; Hippie expansion for word before point
(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'custom-keybind-init)
