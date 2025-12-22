;; https://github.com/purcell/sqlformat
(use-package sqlformat
  :defer t
  :commands (sqlformat sqlformat-buffer)
  :init
  (memacs/minor-leader-def
    :keymaps 'sql-mode-map
    "f" '(sqlformat :which-key "format SQL")))

(provide 'setup-sql)
