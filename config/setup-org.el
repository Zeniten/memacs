(use-package org
  :defer t
  :hook
  (org-mode . (lambda ()
		(setq-local fill-column 80) ; Set fill-column locally for Org-mode
		(auto-fill-mode 1)))
  :custom
  (org-startup-folded t)
  (org-startup-indented t)
  :config
  (which-key-add-key-based-replacements
    ", i" "insert"
    ", b" "babel")
  (my-leader-def
    :keymaps '(org-mode-map)
    "ib" #'org-insert-structure-template
    "il" #'org-insert-link
    "bd" #'org-babel-demarcate-block)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t))))

(use-package org-roam
  :defer t
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Advice to ensure we always switch focus to the org-roam buffer
  (defun my-org-roam-focus-buffer (&rest _)
    "Ensure focus moves to the `*org-roam*` buffer when toggled."
    (when-let ((window (get-buffer-window "*org-roam*")))
      (select-window window))) ;; Explicitly select the `*org-roam*` window

  (advice-add 'org-roam-buffer-toggle :after #'my-org-roam-focus-buffer)

  ;; Enable Org-Roam's database autosync
  (org-roam-db-autosync-mode))

;; org-noter
;; https://github.com/org-noter/org-noter

;; anki-editor
;; https://github.com/anki-editor/anki-editor

;;;; https://github.com/orgtre/ankiorg

(provide 'setup-org)
