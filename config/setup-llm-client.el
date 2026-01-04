(memacs/leader-def
  "l" '(:ignore t :which-key "llm"))

(use-package gptel
  :defer t
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-model 'claude-sonnet-4-5-20250929)
  :init
  (memacs/leader-def
    "lg" '(:ignore t :which-key "gptel")
    "lga" 'gptel-add
    "lgm" 'gptel-menu
    "lgr" 'gptel-rewrite
    "lgs" 'gptel-send)
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key #'gptel-api-key)))

(use-package eca
  :defer t
  :init
  (memacs/leader-def
    "le" '(:ignore t :which-key "eca")
    "lem" 'eca))

(provide 'setup-llm-client)
