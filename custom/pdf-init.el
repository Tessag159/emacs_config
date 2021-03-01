;; PDF configuration

(pdf-tools-install)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(provide 'pdf-init)
