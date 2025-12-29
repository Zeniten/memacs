;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config

(use-package typescript-ts-mode
  :ensure nil
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2))

(provide 'setup-typescript)
