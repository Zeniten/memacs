(use-package org
  :defer t
  :hook
  (org-mode . (lambda ()
                (setq-local fill-column 80) ; Set fill-column locally for Org-mode
                (auto-fill-mode 1)
                (add-hook 'before-save-hook #'whitespace-cleanup nil t)))
  :custom
  (org-startup-folded t)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  ;; doesn't work:
  ;; (org-global-properties
  ;;  '(("Asked" . ":")))
  :config
  (memacs/minor-leader-def
    :keymaps '(org-mode-map)
    "i" '(:ignore t :which-key "insertion")
    "b" '(:ignore t :which-key "babel")
    "ib" #'org-insert-structure-template
    "ih" #'org-insert-heading
    "iH" #'org-insert-heading-after-current
    "il" #'org-insert-link
    "in" #'org-add-note
    "it" #'tempel-insert
    "bd" #'org-babel-demarcate-block)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t))))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autolinks t
        org-appear-inside-latex t))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org/"))
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n g" . org-roam-graph)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n c" . org-roam-capture)
  ;;        ;; Dailies
  ;;        ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Advice to ensure we always switch focus to the org-roam buffer
  (defun memacs/org-roam-focus-buffer (&rest _)
    "Ensure focus moves to the `*org-roam*` buffer when toggled."
    (when-let ((window (get-buffer-window "*org-roam*")))
      (select-window window))) ;; Explicitly select the `*org-roam*` window

  (advice-add 'org-roam-buffer-toggle :after #'memacs/org-roam-focus-buffer)

  (memacs/minor-leader-def
    :keymaps '(org-mode-map)
    "r" '(:ignore t :which-key "org-roam")
    "rc" #'org-roam-capture
    "ri" #'org-roam-node-insert
    "rl" #'org-roam-buffer-toggle
    "rf" #'org-roam-node-find)

  ;; Enable Org-Roam's database autosync
  (org-roam-db-autosync-mode))

;; org-noter
;; https://github.com/org-noter/org-noter

;; anki-editor
;; https://github.com/anki-editor/anki-editor
(use-package anki-editor
  :vc (:url "https://github.com/anki-editor/anki-editor" :rev :newest)
  :defer t
  :config
  (memacs/minor-leader-def
    :keymaps '(org-mode-map)
    "a" '(:ignore t :which-key "anki-editor")
    "ai" #'anki-editor-insert-note
    "ap" #'anki-editor-push-notes))

;;;; https://github.com/orgtre/ankiorg

(provide 'setup-org)
