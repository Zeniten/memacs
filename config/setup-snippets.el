;; LSP servers provides snippets! Some Eglot + TempEL solutions:
;; - https://github.com/svaante/lsp-snippet
;; - https://github.com/fejfighter/eglot-tempel
(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'org-mode-hook #'tempel-setup-capf))

(provide 'setup-snippets)
