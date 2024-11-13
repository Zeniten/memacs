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
	 (clojurec-mode . eglot-ensure)))

(use-package cider
  :defer t)

(provide 'setup-clojure)
