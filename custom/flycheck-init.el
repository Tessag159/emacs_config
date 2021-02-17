;; Flycheck configuration

(add-hook 'c++-mode-hook 'flycheck-mode)
;; This should be set locally for just the one project
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-include-path
			   (list (expand-file-name "/usr/local/include/opencv4")))))
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++14"))
	  )

(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'flycheck-init)
