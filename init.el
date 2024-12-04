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

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

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

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t)
  :config
  (auto-package-update-maybe))

;; idea: https://www.rahuljuliato.com/posts/auto-dark-catppuccin
;; (use-package auto-dark
;;   :custom
;;   (auto-dark-themes '((catppuccin) (catppuccin)))
;;   :hook
;;   (auto-dark-dark-mode
;;    . (lambda ()
;;        (setq catppuccin-flavor 'frappe)
;;        (catppuccin-reload)))
;;   (auto-dark-light-mode
;;    . (lambda ()
;;        (setq catppuccin-flavor 'latte)
;;        (catppuccin-reload)))
;;   :config (auto-dark-mode 1))

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
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package recentf
  :custom
  (recentf-max-saved-items 50) ; show more recent files
  :config
  (recentf-mode 1))

;; Example configuration for Consult
(use-package consult)

(require 'core-memacs)
(require 'setup-general)

(require 'setup-org)
(require 'setup-magit)
(require 'setup-elisp)
(require 'setup-clojure)
(require 'setup-typescript)
(require 'setup-markdown)
