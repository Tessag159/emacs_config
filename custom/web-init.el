;; Web development configuration

(use-package shr-tag-pre-highlight)

(use-package impatient-mode
  :hook
  (html-mode . impatient-mode)
  (js-mode . impatient-mode)
  (css-mode . impatient-mode)
  :config
  (httpd-start))



(provide 'web-init)
