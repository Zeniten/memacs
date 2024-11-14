(use-package smartparens
  :defer t
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package clojure-mode
  :after evil ; necessary?
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.cljc\\'" . clojurec-mode))
  :config
  ;; is this conventional? would general.el be better?
  ;; what does spacemacs do?
  (evil-set-leader '(normal visual) (kbd ",") t)
  (evil-define-key '(normal visual) 'local
    (kbd ",ef") 'cider-eval-defun-at-point
    (kbd ",es") 'cider-eval-sexp-at-point)
  :hook ((clojure-mode . eglot-ensure)
	 (clojurescript-mode . eglot-ensure)
	 (clojurec-mode . eglot-ensure)))

(use-package eldoc
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package cider
  :defer t)

(provide 'setup-clojure)
