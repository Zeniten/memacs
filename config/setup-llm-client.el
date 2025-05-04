(use-package gptel
  :defer t
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-model 'gpt-4o)
  :init
  (memacs/leader-def
    "l" '(:ignore t :which-key "llm")
    "la" 'gptel-add
    "lm" 'gptel-menu
    "lr" 'gptel-rewrite
    "ls" 'gptel-send))

(use-package aidermacs
  :defer t
  :init
  (memacs/leader-def
    "le" 'aidermacs-transient-menu))

(provide 'setup-llm-client)
