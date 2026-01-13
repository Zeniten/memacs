(use-package emacs
  :ensure nil
  :custom
  ;; scrolling
  (scroll-conservatively 1000) ;; ensure cursor keeps its placement when jumping to end of buffer
  (scroll-margin 3)
  (scroll-step 1)

  ;; helping
  (help-window-select t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :ensure nil
  :defer t
  :hook (eglot-managed-mode . (lambda ()
				(setq-local eldoc-documentation-functions
					    (seq-difference eldoc-documentation-functions
							    '(eglot-signature-eldoc-function
							      eglot-highlight-eldoc-function)))))
  :custom
  (eglot-connect-timeout 120)
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  (with-eval-after-load 'general
    (memacs/minor-leader-def
      :keymaps 'eglot-mode-map
      "l"  '(:ignore t :which-key "lsp")
      "lr" '(eglot-rename :which-key "rename")
      "la" '(eglot-code-actions :which-key "code actions")
      "lf" '(eglot-format-buffer :which-key "format buffer"))))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\Dockerfile\\'" . dockerfile-ts-mode))
  :preface
  (defun memacs/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (bash-mode . bash-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (memacs/setup-install-grammars))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  ;; https://github.com/emacs-evil/evil-cleverparens?tab=readme-ov-file
  (evil-move-beyond-eol t)
  :hook (prog-mode . hs-minor-mode)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

;; https://github.com/jamescherti/enhanced-evil-paredit.el

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init)
  :config
  (evil-set-initial-state 'Info-mode 'emacs)

  ;; Rebind SPC in Dired mode
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      (kbd "<SPC>") nil)
    (define-key dired-mode-map (kbd "<SPC>") (lookup-key (current-global-map) (kbd "<SPC>")))))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-define-key '(normal visual) 'global
    "gc" #'evilnc-comment-operator))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package modus-themes
  :custom
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :config
  (load-theme 'modus-operandi-tinted))

;; https://github.com/syl20bnr/spacemacs/blob/b0591a8ba9b4709bb18b354ae162400852378114/layers/%2Bspacemacs/spacemacs-editing/packages.el#L175
(use-package expand-region
  :custom
  (expand-region-contract-fast-key "V")
  (expand-region-reset-fast-key "r"))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

(use-package winum
  :init
  (winum-mode 1))

(use-package dired-sidebar
  :custom
  (dired-sidebar-subtree-line-prefix "•")
  :commands (dired-sidebar-toggle-sidebar)
  :hook (dired-sidebar-mode . (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(defun memacs/split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun memacs/split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (other-window 1))

(evil-define-key '(normal visual) prog-mode-map
  (kbd "TAB") #'indent-for-tab-command)

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-display-functions '(eldoc-display-in-buffer))
  :config
  ;; Auto-focus the eldoc buffer when displayed
  (advice-add 'eldoc-doc-buffer :after
              (lambda (&rest _)
                (when-let ((buf (get-buffer "*eldoc*")))
                  (when (get-buffer-window buf)
                    (select-window (get-buffer-window buf)))))))

(use-package esup
  :defer t)

(use-package auto-dark
  :custom
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
  :init
  (auto-dark-mode))

(use-package try)

(provide 'core-memacs)
