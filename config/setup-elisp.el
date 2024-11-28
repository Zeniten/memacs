(use-package eros
  :config
  (my-leader-def
    :keymaps 'emacs-lisp-mode-map
    "ef" 'eros-eval-defun
    "eb" 'eros-eval-buffer)
  (which-key-add-key-based-replacements
    ", e" "evaluation")
  (eros-mode 1))

(provide 'setup-elisp)
