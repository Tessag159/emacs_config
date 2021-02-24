;; Flycheck configuration

(use-package flycheck
  :hook
  (c++-mode . flycheck-mode)
  (c++-mode . (lambda ()
		(setq flycheck-clang-language-standard="c++14")))
  (c++-mode . (lambda ()
		(setq flycheck-clang-include-path (list (expand-file-name "/usr/local/include/opencv4")))))
  (python-mode . flycheck-mode))


(provide 'flycheck-init)
