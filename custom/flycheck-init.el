;; Flycheck configuration

(defun ti/set-flycheck-clang-standard-c++ ()
  "Sets the language standard flycheck should use for c++"
  (setq flycheck-clang-language-standard "c++14"))

(defun ti/set-flycheck-include-path-c++ ()
  "Sets the include path for flycheck to use for my current c++ project"
  (setq flycheck-clang-include-path
	(list (expand-file-name "/usr/local/include/opencv4"))))


(use-package flycheck
  :hook
  (c++-mode . flycheck-mode)
  (c++-mode . ti/set-flycheck-clang-standard-c++)
  (c++-mode . ti/set-flycheck-clang-include-path-c++)
  (python-mode . flycheck-mode))


(provide 'flycheck-init)
