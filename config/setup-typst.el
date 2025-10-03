;; https://codeberg.org/meow_king/typst-ts-mode
(use-package typst-ts-mode
  ;; :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode" :branch "develop")
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode" :rev :newest)
  :mode (("\\.typ\\'" . typst-ts-mode))
  )

(provide 'setup-typst)
