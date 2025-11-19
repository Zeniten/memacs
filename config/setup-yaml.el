(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :mode (("\\.ya?ml\\'" . yaml-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  )

(provide 'setup-yaml)
