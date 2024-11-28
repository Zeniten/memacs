(use-package elisp-mode
  :ensure nil
  :config
  (my-leader-def
    :keymaps 'emacs-lisp-mode-map
    "ef" 'eval-defun
    "eb" 'eval-buffer)
  (which-key-add-key-based-replacements
    ", e" "evaluation"))

(provide 'setup-elisp)
