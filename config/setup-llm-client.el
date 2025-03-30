(use-package gptel
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-model 'gpt-4o)
  :config
  (memacs/leader-def
    "l" '(:ignore t :which-key "llm")
    "la" 'gptel-add
    "lm" 'gptel-menu
    "lr" 'gptel-rewrite
    "ls" 'gptel-send))

(use-package aidermacs
  :config
  (memacs/leader-def
    "le" 'aidermacs-transient-menu))

(provide 'setup-llm-client)
