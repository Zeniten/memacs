(use-package markdown-mode
  :mode
  (("\\.mdx\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc"))

(provide 'setup-markdown)
