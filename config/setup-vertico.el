(use-package vertico
  :config
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (vertico-mode 1))

;; Vertico directory extension for better file path navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Save minibuffer history
(use-package savehist
  :config
  (savehist-mode 1))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Optionally use the `orderless' completion style: order of expressions doesn't matter for search.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  :config
  (recentf-mode 1))

;; Enhanced Consult configuration
(use-package consult
  :init
  ;; Use Consult for xref (better jump-to-definition UI)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure preview
  (setq consult-preview-key 'any
        consult-narrow-key "<")

  :config
  ;; Project root function for project-aware completions
  (setq consult-project-function
        (lambda (_)
          (when-let ((project (project-current)))
            (project-root project)))))

(provide 'setup-vertico)
