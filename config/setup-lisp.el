(use-package smartparens
  :defer t
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . smartparens-strict-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (memacs/leader-def
    :keymaps 'smartparens-strict-mode-map
    "k"  '(:ignore t :which-key "lisp")
    "kw" '(sp-wrap-round :which-key "wrap round")
    "k(" '(sp-wrap-round :which-key "wrap round (")
    "k)" '(sp-wrap-round :which-key "wrap round )")
    "k[" '(sp-wrap-square :which-key "wrap square [")
    "k]" '(sp-wrap-square :which-key "wrap square ]")
    "k{" '(sp-wrap-curly :which-key "wrap curly {")
    "k}" '(sp-wrap-curly :which-key "wrap curly }")
    "kr" '(sp-raise-sexp :which-key "raise sexp")
    "kt" '(sp-transpose-sexp :which-key "transpose sexp")
    "ks" '(sp-forward-slurp-sexp :which-key "slurp forward")
    "kS" '(sp-backward-slurp-sexp :which-key "slurp backward")
    "kb" '(sp-forward-barf-sexp :which-key "barf forward")
    "kB" '(sp-backward-barf-sexp :which-key "barf backward")))

(use-package evil-cleverparens
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . evil-cleverparens-mode))

(provide 'setup-lisp)
