(use-package eros
  :config
  (my-leader-def
    :keymaps 'emacs-lisp-mode-map
    "eb" 'eval-buffer
    "ef" 'eros-eval-defun
    "es" 'eros-eval-last-sexp)
  (which-key-add-key-based-replacements
    ", e" "evaluation")
  (eros-mode 1))

(provide 'setup-elisp)
