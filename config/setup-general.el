(use-package general
  :after evil
  :config
  ;; Create a reusable leader definer
  (general-create-definer my-leader-def
    :prefix ","     ;; Leader key
    :states '(normal visual))) ;; Designed for normal and visual states

(provide 'setup-general)
