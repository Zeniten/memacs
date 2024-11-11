;; Lisp
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-ts-mode))

;; Clojure
;; TODO Just testing so far; no idea if this is correct
(use-package clojure-ts-mode
  :hook ((clojure-ts-mode . eglot-ensure)
	 (clojure-ts-mode . corfu-mode)))

(use-package cider)

(provide 'setup-clojure)
