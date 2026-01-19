(use-package smartparens
  :pin "MELPA"
  :defer t
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . smartparens-strict-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package evil-cleverparens
  :hook ((emacs-lisp-mode clojure-mode clojurescript-mode clojurec-mode) . evil-cleverparens-mode))

(provide 'setup-lisp)
