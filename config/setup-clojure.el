(use-package smartparens
  :defer t
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.cljc\\'" . clojurec-mode)))

;(use-package eglot
;  :defer t
;  :hook (clojure-mode clojurescript-mode clojurec-mode))

(use-package cider
  :defer t)

(provide 'setup-clojure)
