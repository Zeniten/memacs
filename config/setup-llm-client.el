(memacs/leader-def
  "l" '(:ignore t :which-key "llm"))

(use-package gptel
  :defer t
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-model 'gpt-4o)
  :init
  (memacs/leader-def
    "lg" '(:ignore t :which-key "gptel")
    "lga" 'gptel-add
    "lgm" 'gptel-menu
    "lgr" 'gptel-rewrite
    "lgs" 'gptel-send))

(use-package eca
  :defer t
  :init
  (memacs/leader-def
    "le" '(:ignore t :which-key "eca")
    "lem" 'eca))

(provide 'setup-llm-client)
