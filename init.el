;; memacs
;; alias for: "emacs --init-dir ~/.config/emacs/"


;; Keep tabs on startup time

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; start emacs in fullscreen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)


(set-language-environment "UTF-8")
;; explicitly set the preferred coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; TODO Set only for certain modes

(add-hook 'after-init-hook
             (lambda ()
               (setq display-line-numbers-type 'relative)
               (add-hook 'prog-mode-hook 'display-line-numbers-mode)))


;; PERF: Shave seconds off startup time by starting the scratch buffer in
;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
;;   pull in a ton of packages. `doom/open-scratch-buffer' provides a better
;;   scratch buffer anyway.
(setq frame-inhibit-implied-resize 1 ; don't resize frame implicitly
      inhibit-startup-screen t  ; skip startup screen
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil ; clean scratch buffer
      recentf-max-saved-items 1000 ; show more recent files
      ring-bell-function 'ignore ; quiet
      sentence-end-double-space nil
      )

;; Some variables are buffer-local, so changing them using setq will only change
;; them in a single buffer. Using setq-default, we change the buffer-local variable's
;; default vaule.
(setq-default fill-column 80)

;; Fonts
(set-face-attribute 'default nil :font "Fira Code"
		                 :height 113)

;; Answering yes and no to each question from Emacs can be tedious, a single y or n will suffice.
(fset 'yes-or-no-p 'y-or-n-p)

(load-theme 'deeper-blue t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;(defvar space-map (make-sparse-keymap)
;  "Keymap for SPC commands.")

;(define-key space-map (kbd "gs") 'magit-status)
;(global-set-key (kbd "SPC") space-map)

;; use use-package
(require 'use-package)
(setq use-package-always-ensure t) ; download package if not there

;; set places to find packages
;; TODO Hvorfor har du ikke med nongnu elpa?
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 0)))

(use-package vertico
  :config
  (vertico-mode 1))

;; Save minibuffer history
(use-package savehist
  :config
  (savehist-mode 1))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :config 
  (marginalia-mode 1))

;; Optionally use the `orderless' completion style: order of expressions doesn't matter for search.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package which-key
  :init
  (which-key-mode 1))

;; Winum
(use-package winum
  :init
  (winum-mode 1))

;; Evil
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

(use-package magit
  :defer t)

;; https://github.com/noctuid/evil-guide#keybindings-and-states
(evil-define-key '(normal visual) 'global
  (kbd "<SPC>fs") 'save-buffer
  (kbd "<SPC>ff") 'find-file
  (kbd "<SPC>fj") 'dired-jump
  (kbd "<SPC>qq") 'kill-emacs
  (kbd "<SPC>qr") 'restart-emacs
  (kbd "<SPC>SPC") 'execute-extended-command ;meta

  (kbd "<SPC>pp") 'project-switch-project
  (kbd "<SPC>pf") 'project-find-file

  (kbd "<SPC>ss") 'consult-line

  (kbd "<SPC>gs") 'magit-status

  (kbd "<SPC>bp") 'previous-buffer
  (kbd "<SPC>bn") 'next-buffer
  (kbd "<SPC>bb") 'consult-buffer

  (kbd "<SPC>wv") 'split-window-right
  (kbd "<SPC>wV") 'split-window-right-and-focus
  (kbd "<SPC>ws") 'split-window-below
  (kbd "<SPC>wS") 'split-window-below-and-focus
  (kbd "<SPC>wK") 'evil-window-move-very-top
  (kbd "<SPC>wH") 'evil-window-move-far-left
  (kbd "<SPC>wJ") 'evil-window-move-very-bottom
  (kbd "<SPC>wL") 'evil-window-move-far-right
  (kbd "<SPC>wd") 'delete-window
  (kbd "<SPC>1") 'winum-select-window-1
  (kbd "<SPC>2") 'winum-select-window-2
  (kbd "<SPC>3") 'winum-select-window-3
  (kbd "<SPC>4") 'winum-select-window-4
  (kbd "<SPC>5") 'winum-select-window-5
  (kbd "<SPC>6") 'winum-select-window-6
  (kbd "<SPC>7") 'winum-select-window-7
  (kbd "<SPC>8") 'winum-select-window-8
  (kbd "<SPC>9") 'winum-select-window-9
  )

;; Lisp
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; TODO Is this the way to connect clj files to clojure-ts-mode?
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))
;; Clojure
;; TODO Just testing so far; no idea if this is correct
(use-package clojure-ts-mode
  :hook ((clojure-ts-mode . eglot-ensure)
	 (clojure-ts-mode . corfu-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" default))
 '(nano-modeline-position 'nano-modeline-footer)
 '(package-selected-packages
   '(clojure-ts-mode corfu smartparens-mode orderless zenburn-theme marginalia nano-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
