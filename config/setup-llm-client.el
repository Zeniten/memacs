(use-package gptel
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-model 'gpt-4o)
  :config
  (which-key-add-key-based-replacements
    "<SPC> l" "llm")
  (evil-define-key '(normal visual) 'global
    (kbd "<SPC>la") #'gptel-add  ; add context
    (kbd "<SPC>lm") #'gptel-menu ; transient menu for preference, etc.
    (kbd "<SPC>lr") #'gptel-rewrite
    (kbd "<SPC>ls") #'gptel-send)
  )

(provide 'setup-llm-client)
