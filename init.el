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

(setq sentence-end-double-space nil)

;; Some variables are buffer-local, so changing them using setq will only change
;; them in a single buffer. Using setq-default, we change the buffer-local variable's
;; default vaule.
(setq-default fill-column 80)

;; Use EWW for in-Emacs browsing (e.g., cider-javadoc)
(setq browse-url-browser-function 'eww-browse-url)

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

(require 'core-memacs)
(require 'setup-font)
(require 'setup-project)
(require 'setup-general)
(require 'setup-lisp)

(require 'setup-vertico)
(require 'setup-snippets)

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
