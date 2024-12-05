(use-package emacs
  :custom
  ;; scrolling
  (scroll-conservatively 1000) ;; ensure cursor keeps its placement when jumping to end of buffer
  (scroll-margin 3)
  (scroll-step 1)

  ;; helping
  (help-window-select t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.4
	which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

(use-package winum
  :init
  (winum-mode 1))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'latte)
  :config
  (load-theme 'catppuccin :no-confirm)

  (defvar my-catppuccin-themes '(latte frappe))

  (defun cycle-themes ()
    "Cycle through Catppuccin theme flavors defined in `my-catppuccin-themes`."
    (interactive)
    (let ((rotated (nconc (cdr my-catppuccin-themes) (list (car my-catppuccin-themes)))))
      (setq catppuccin-flavor (car (setq my-catppuccin-themes rotated)))
      (load-theme 'catppuccin :no-confirm)
      (message "Switched to Catppuccin theme: %s" (symbol-name catppuccin-flavor))))

  ;; Load fallback theme if Catppuccin fails
  (add-hook 'after-init-hook
            (lambda ()
              (unless (featurep 'catppuccin-theme)
                (load-theme 'deeper-blue t))))

  (which-key-add-key-based-replacements
    "<SPC> t" "theme"
    )
  (evil-define-key '(normal visual) 'global
    (kbd "<SPC>tc") #'cycle-themes)

  ;; TODO What if Catppuccin fails?
  ;(global-set-key (kbd "C-c t") 'cycle-themes)
  )

(which-key-add-key-based-replacements
  "<SPC> f" "file"
  "<SPC> h" "help"
  "<SPC> p" "project"
  "<SPC> s" "search"
  "<SPC> g" "git"
  "<SPC> b" "buffer"
  "<SPC> w" "window"
  "<SPC> 1" "window 1"
  "<SPC> 2" "window 2"
  "<SPC> 3" "window 3"
  "<SPC> 4" "window 4"
  "<SPC> 5" "window 5"
  "<SPC> 6" "window 6"
  "<SPC> 7" "window 7"
  "<SPC> 8" "window 8"
  "<SPC> 9" "window 9"
  "<SPC> q" "quit/restart"
  "<SPC> SPC" "M-x"
)

;; https://github.com/noctuid/evil-guide#keybindings-and-states
(evil-define-key '(normal visual) 'global
  (kbd "<SPC>fs") #'save-buffer
  (kbd "<SPC>ff") #'find-file
  (kbd "<SPC>fj") #'dired-jump
  (kbd "<SPC>qq") #'kill-emacs
  (kbd "<SPC>qr") #'restart-emacs
  (kbd "<SPC>SPC") #'execute-extended-command ;meta

  (kbd "<SPC>pp") #'project-switch-project
  (kbd "<SPC>pf") #'project-find-file
  (kbd "<SPC>/") #'consult-ripgrep

  (kbd "<SPC>ss") #'consult-line

  (kbd "<SPC>gs") #'magit-status

  (kbd "<SPC>hf") #'describe-function
  (kbd "<SPC>hk") #'describe-key
  (kbd "<SPC>hm") #'describe-mode
  (kbd "<SPC>hv") #'describe-variable

  (kbd "<SPC>bp") #'previous-buffer
  (kbd "<SPC>bn") #'next-buffer
  (kbd "<SPC>bb") #'consult-buffer

  (kbd "<SPC>wv") #'split-window-right
  (kbd "<SPC>wV") #'split-window-right-and-focus
  (kbd "<SPC>ws") #'split-window-below
  (kbd "<SPC>wS") #'split-window-below-and-focus
  (kbd "<SPC>wK") #'evil-window-move-very-top
  (kbd "<SPC>wH") #'evil-window-move-far-left
  (kbd "<SPC>wJ") #'evil-window-move-very-bottom
  (kbd "<SPC>wL") #'evil-window-move-far-right
  (kbd "<SPC>wd") #'delete-window
  (kbd "<SPC>1") #'winum-select-window-1
  (kbd "<SPC>2") #'winum-select-window-2
  (kbd "<SPC>3") #'winum-select-window-3
  (kbd "<SPC>4") #'winum-select-window-4
  (kbd "<SPC>5") #'winum-select-window-5
  (kbd "<SPC>6") #'winum-select-window-6
  (kbd "<SPC>7") #'winum-select-window-7
  (kbd "<SPC>8") #'winum-select-window-8
  (kbd "<SPC>9") #'winum-select-window-9
  )

(evil-define-key '(normal visual) prog-mode-map
  (kbd "TAB") #'indent-for-tab-command
  )

(use-package esup
  :defer)

(provide 'core-memacs)
