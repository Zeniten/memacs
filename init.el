;; memacs
;; alias for: "emacs --init-dir ~/.config/emacs/"

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

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

;; Display relative line numbers in prog-mode
(add-hook 'after-init-hook
	  (lambda ()
	    (setq display-line-numbers-type 'relative)
	    (add-hook 'prog-mode-hook 'display-line-numbers-mode)))

;; Workaround for transient bug with overriding-text-conversion-style
;; Fixes "Symbol's value as variable is void" error when using transient
;; (e.g., M-x guix, M-x magit) in certain buffer contexts like Git repos
;; Define the variable if it doesn't exist in this Emacs build
(unless (boundp 'overriding-text-conversion-style)
  (defvar overriding-text-conversion-style nil
    "Text conversion style override (not present in all Emacs builds)."))

;; PERF: Shave seconds off startup time by starting the scratch buffer in
;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
;;   pull in a ton of packages. `doom/open-scratch-buffer' provides a better
;;   scratch buffer anyway.
(setq sentence-end-double-space nil)

;; Some variables are buffer-local, so changing them using setq will only change
;; them in a single buffer. Using setq-default, we change the buffer-local variable's
;; default vaule.
(setq-default fill-column 80)

;; Use EWW for in-Emacs browsing (e.g., cider-javadoc)
(setq browse-url-browser-function 'eww-browse-url)

;; Fonts
(add-hook 'after-init-hook
	  (lambda ()
	    (set-face-attribute 'default nil :font "Fira Code" :height 113)))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Answering yes and no to each question from Emacs can be tedious, a single y or n will suffice.
(setq use-short-answers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Disable lock files (recommended for single-user systems)
(setq create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; use use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t)

;; set places to find packages
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("NonGNU ELPA"  . "https://elpa.gnu.org/nongnu/")
	("MELPA Stable" . "https://stable.melpa.org/packages/")
	("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 4)
	("NonGNU ELPA"  . 3)
	("MELPA"        . 2)
	("MELPA Stable" . 1)))

(use-package vertico
  :config
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (vertico-mode 1))

;; Vertico directory extension for better file path navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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

;; LSP servers provides snippets! Some Eglot + TempEL solutions:
;; - https://github.com/svaante/lsp-snippet
;; - https://github.com/fejfighter/eglot-tempel
(use-package tempel
  :bind (("M-+" . tempel-complete)
	 ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'org-mode-hook #'tempel-setup-capf))

(use-package corfu
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  :config
  (recentf-mode 1))

;; Enhanced Consult configuration
(use-package consult
  :init
  ;; Use Consult for xref (better jump-to-definition UI)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure preview
  (setq consult-preview-key 'any
        consult-narrow-key "<")

  :config
  ;; Project root function for project-aware completions
  (setq consult-project-function
        (lambda (_)
          (when-let ((project (project-current)))
            (project-root project)))))

(require 'core-memacs)
(require 'setup-general)

(require 'setup-org)
(require 'setup-magit)
(require 'setup-elisp)
(require 'setup-guix)
(require 'setup-llm-client)
(require 'setup-rest-client)
(require 'setup-clojure)
(require 'setup-typescript)
(require 'setup-markdown)
(require 'setup-sql)
(require 'setup-docker)
(require 'setup-yaml)
(require 'setup-typst)
(require 'setup-converters)
