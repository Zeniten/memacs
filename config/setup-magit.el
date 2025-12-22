(use-package magit
  :defer t)

(use-package git-timemachine
  :defer t
  :init
  (memacs/leader-def
    "gt" '(git-timemachine :which-key "git time machine")))

(provide 'setup-magit)
