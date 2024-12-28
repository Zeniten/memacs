(use-package smartparens
  :defer t
  :hook (prog-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (which-key-add-key-based-replacements
    "<SPC> k" "lisp")
  (evil-define-key '(normal visual) 'global
    (kbd "<SPC>kw") #'sp-wrap-round
    (kbd "<SPC>k(") #'sp-wrap-round
    (kbd "<SPC>k)") #'sp-wrap-round
    (kbd "<SPC>k[") #'sp-wrap-square
    (kbd "<SPC>k]") #'sp-wrap-square
    (kbd "<SPC>k{") #'sp-wrap-curly
    (kbd "<SPC>k}") #'sp-wrap-curly
    (kbd "<SPC>kr") #'sp-raise-sexp
    (kbd "<SPC>kt") #'sp-transpose-sexp
    (kbd "<SPC>ks") #'sp-forward-slurp-sexp
    (kbd "<SPC>kS") #'sp-backward-slurp-sexp
    (kbd "<SPC>kb") #'sp-forward-barf-sexp
    (kbd "<SPC>kB") #'sp-backward-barf-sexp))

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
    "eb" #'cider-load-buffer
    "ef" #'cider-eval-defun-at-point
    "el" #'cider-eval-list-at-point
    "es" #'cider-eval-sexp-at-point
    "rr" #'eglot-rename
    "sqq" #'cider-quit)
  (which-key-add-key-based-replacements
    ", e" "evaluation"
    ", r" "refactor"
    ", s" "send to repl"
    ", sq" "quit/restart repl"
    ))

(use-package eldoc
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package cider
  :defer t
  :custom
  (cider-repl-pop-to-buffer-on-connect nil))

(provide 'setup-clojure)
