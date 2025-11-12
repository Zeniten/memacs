;; (use-package dockerfile-mode
;;   :defer t)

(use-package docker
  :defer t
  :init
  (memacs/leader-def
    "d" 'docker))

(provide 'setup-docker)
