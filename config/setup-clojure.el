(use-package smartparens
  :defer t
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure))
  :config
  (my-leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
    "eb" 'cider-load-buffer
    "ef" 'cider-eval-defun-at-point
    "el" 'cider-eval-list-at-point
    "es" 'cider-eval-sexp-at-point)
  (which-key-add-key-based-replacements
    ", e" "evaluation"))

(use-package eldoc
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package cider
  :defer t)

(provide 'setup-clojure)
