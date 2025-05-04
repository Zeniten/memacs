(use-package emacs
  :ensure nil
  :custom
  ;; scrolling
  (scroll-conservatively 1000) ;; ensure cursor keeps its placement when jumping to end of buffer
  (scroll-margin 3)
  (scroll-step 1)

  ;; helping
  (help-window-select t))

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-events-buffer-config '(:size 0 :format full)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  ;; https://github.com/emacs-evil/evil-cleverparens?tab=readme-ov-file
  (evil-move-beyond-eol t)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

;; https://github.com/jamescherti/enhanced-evil-paredit.el

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init)
  :config
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

;; how to achieve: select window 1..9
;; (with-eval-after-load 'winum
;;   (setq winum-keymap
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map (kbd "C-x w 1") 'winum-select-window-1)
;;           (define-key map (kbd "C-x w 2") 'winum-select-window-2)
;;           (define-key map (kbd "C-x w 3") 'winum-select-window-3)
;;           (define-key map (kbd "C-x w 4") 'winum-select-window-4)
;;           (define-key map (kbd "C-x w 5") 'winum-select-window-5)
;;           (define-key map (kbd "C-x w 6") 'winum-select-window-6)
;;           (define-key map (kbd "C-x w 7") 'winum-select-window-7)
;;           (define-key map (kbd "C-x w 8") 'winum-select-window-8)
;;           (define-key map (kbd "C-x w 9") 'winum-select-window-9)
;;           map)))

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
  (eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package esup
  :defer t)

(use-package auto-dark
  :custom
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
  :init (auto-dark-mode))

(use-package try)

(provide 'core-memacs)
