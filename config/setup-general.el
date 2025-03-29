(use-package general
  :after evil
  :config
  ;; Create a reusable leader definer for <SPC>
  (general-create-definer memacs/leader-def
    :prefix "SPC" ;; Leader key
    :keymaps 'override ;; Use 'override to ensure these bindings take precedence
    :states '(normal visual motion emacs) ;; Apply to multiple states
    :non-normal-prefix "M-SPC") ;; Optional: prefix for non-normal states like Emacs state

  (memacs/leader-def
    ;; File operations
    "f"  '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save buffer")
    "fS" '(evil-write-all :which-key "save all buffers")
    "ff" '(find-file :which-key "find file")
    "fj" '(dired-jump :which-key "jump to directory")

    ;; Quit/Restart
    "q"  '(:ignore t :which-key "quit/restart")
    "qq" '(kill-emacs :which-key "quit emacs")
    "qr" '(restart-emacs :which-key "restart emacs")

    ;; Execute command
    "SPC" '(execute-extended-command :which-key "M-x")

    ;; Project management
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file in project")

    ;; Search
    "s"  '(:ignore t :which-key "search")
    "/"  '(consult-ripgrep :which-key "search project (consult-ripgrep)") ; Note: "/" directly under SPC
    "ss" '(consult-line :which-key "search current buffer (consult-line)")

    ;; Git
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "magit status")

    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hi" '(info :which-key "info")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")
    "hv" '(describe-variable :which-key "describe variable")

    ;; Buffer management
    "b"  '(:ignore t :which-key "buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bb" '(consult-buffer :which-key "switch buffer (consult)")

    ;; Window management
    "w"  '(:ignore t :which-key "window")
    "wv" '(split-window-right :which-key "split right")
    "wV" '(memacs/split-window-right-and-focus :which-key "split right and focus")
    "ws" '(split-window-below :which-key "split below")
    "wS" '(memacs/split-window-below-and-focus :which-key "split below and focus")
    "wK" '(evil-window-move-very-top :which-key "move window top")
    "wH" '(evil-window-move-far-left :which-key "move window left")
    "wJ" '(evil-window-move-very-bottom :which-key "move window bottom")
    "wL" '(evil-window-move-far-right :which-key "move window right")
    "wd" '(delete-window :which-key "delete window")

    ;; Window selection (Winum)
    "1" '(winum-select-window-1 :which-key "select window 1")
    "2" '(winum-select-window-2 :which-key "select window 2")
    "3" '(winum-select-window-3 :which-key "select window 3")
    "4" '(winum-select-window-4 :which-key "select window 4")
    "5" '(winum-select-window-5 :which-key "select window 5")
    "6" '(winum-select-window-6 :which-key "select window 6")
    "7" '(winum-select-window-7 :which-key "select window 7")
    "8" '(winum-select-window-8 :which-key "select window 8")
    "9" '(winum-select-window-9 :which-key "select window 9")

    ;; Theme toggle
    "t"  '(:ignore t :which-key "theme")
    "tt" '(modus-themes-toggle :which-key "toggle theme")

    ;; Expand region
    "v" '(er/expand-region :which-key "expand region")
    )

(provide 'setup-general)
