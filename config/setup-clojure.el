(use-package smartparens
  :defer t
  :hook (prog-mode text-mode) ;; add `smartparens-mode` to these hooks
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
    "'" #'sesman-start
    "eb" 'cider-load-buffer
    "ef" 'cider-eval-defun-at-point
    "el" 'cider-eval-list-at-point
    "es" 'cider-eval-sexp-at-point

    "sqq" #'cider-quit)
  (which-key-add-key-based-replacements
    ", e" "evaluation"
    ", s" "send to repl"
    ", s q" "quit/restart repl"
    ))

(use-package eldoc
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package cider
  :defer t)

(provide 'setup-clojure)
