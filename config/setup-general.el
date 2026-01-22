(use-package general
  :after evil
  :config
  ;; Create a reusable leader definer for <SPC>
  (general-create-definer memacs/leader-def
    :prefix "SPC" ;; Leader key
    :keymaps 'override ;; Use 'override to ensure these bindings take precedence
    :states '(normal visual) ;; Apply to multiple states
    :non-normal-prefix "M-SPC") ;; Optional: prefix for non-normal states like Emacs state

  (general-create-definer memacs/minor-leader-def
    :prefix ","     ;; Leader key
    :keymaps 'override
    :states '(normal visual))

  (memacs/leader-def
    ;; File operations
    "f"  '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save buffer")
    "fS" '(evil-write-all :which-key "save all buffers")
    "ff" '(find-file :which-key "find file")
    "fj" '(dired-jump :which-key "jump to directory")
    "fr" '(consult-recent-file :which-key "recent files")

    ;; Insertion
    "i" '(:ignore t :which-key "insert")
    "is" '(tempel-insert :which-key "snippet")

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
    "pt" '(dired-sidebar-toggle-sidebar :which-key "toggle sidebar")
    "pa" '(memacs/project-toggle-implementation-test :which-key "toggle impl/test")
    "pb" '(project-switch-to-buffer :which-key "project buffer")
    "pk" '(project-kill-buffers :which-key "kill project buffers")
    "pd" '(project-dired :which-key "project dired")

    ;; Search
    "s"  '(:ignore t :which-key "search")
    "/"  '(consult-ripgrep :which-key "search project (consult-ripgrep)") ; Note: "/" directly under SPC
    "ss" '(consult-line :which-key "search current buffer (consult-line)")
    "si" '(consult-imenu :which-key "imenu")
    "so" '(consult-outline :which-key "outline")

    ;; Org
    "o" '(:ignore t :which-key "org")
    "or" '(:ignore t :which-key "org-roam")
    "orc" #'org-roam-capture
    "orf" #'org-roam-node-find

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
    "hp" '(describe-package :which-key "describe package")
    "hb" '(describe-bindings :which-key "describe bindings")

    ;; Buffer management
    "b"  '(:ignore t :which-key "buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bb" '(consult-buffer :which-key "switch buffer (consult)")
    "bk" '(kill-buffer :which-key "kill buffer")

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

    ;; Window selection
    "0" 'winum-select-window-0-or-10
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9

    ;; Theme toggle
    "t"  '(:ignore t :which-key "theme")
    "tt" '(modus-themes-toggle :which-key "toggle theme")

    ;; Expand region
    "v" '(er/expand-region :which-key "expand region"))

  ;; Hide winum keybindings from which-key
  (dolist (fname '("winum-select-window-0-or-10"
                   "winum-select-window-1"
                   "winum-select-window-2"
                   "winum-select-window-3"
                   "winum-select-window-4"
                   "winum-select-window-5"
                   "winum-select-window-6"
                   "winum-select-window-7"
                   "winum-select-window-8"
                   "winum-select-window-9"))
    (push `((nil . ,fname) . (lambda (kb) nil))
          which-key-replacement-alist)))

(provide 'setup-general)
