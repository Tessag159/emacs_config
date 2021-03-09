;; Flycheck configuration

(use-package flycheck
  :hook
  (c++-mode . flycheck-mode)
  (c++-mode . (lambda ()
		(setq flycheck-clang-language-standard="c++17")))
  (c++-mode . (lambda ()
		(setq flycheck-clang-include-path (list (expand-file-name "/usr/local/include/opencv4"))))))


(provide 'flycheck-init)
