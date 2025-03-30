(defun memacs/cursor-at-parenthesis-p ()
  "Return non-nil if the cursor is over a parenthesis."
  (let ((syntax (syntax-class (syntax-after (point)))))
    (or (eq syntax (car (string-to-syntax "(")))
        (eq syntax (car (string-to-syntax ")"))))))

(defun memacs/sp-move-sexp-up ()
  "Move the current s-expression up."
  (interactive)
  (if (looking-back "(")
      (message "Cannot move past superior level")
    (progn (forward-sexp)
	   (transpose-sexps -1)
	   (backward-sexp))))

(defun memacs/sp-move-sexp-down ()
  "Move the current s-expression down."
  (interactive)
  (forward-sexp)
  (if (memacs/cursor-at-parenthesis-p)
      (progn (message "Cannot move past superior level")
	     (backward-sexp))
    (progn (transpose-sexps 1)
	   (backward-sexp))))

(use-package smartparens
  :defer t
  :hook ((emacs-lisp-mode . smartparens-mode)
	 (clojure-mode . smartparens-mode)
	 (clojurescript-mode . smartparens-mode)
	 (clojure-mode . smartparens-mode))
  :config
  ;; load default config
  (require 'smartparens-config)
  (memacs/leader-def
    "k"  '(:ignore t :which-key "lisp")
    "kw" '(sp-wrap-round :which-key "wrap round")
    "k(" '(sp-wrap-round :which-key "wrap round (")
    "k)" '(sp-wrap-round :which-key "wrap round )")
    "k[" '(sp-wrap-square :which-key "wrap square [")
    "k]" '(sp-wrap-square :which-key "wrap square ]")
    "k{" '(sp-wrap-curly :which-key "wrap curly {")
    "k}" '(sp-wrap-curly :which-key "wrap curly }")
    "kr" '(sp-raise-sexp :which-key "raise sexp")
    "kt" '(sp-transpose-sexp :which-key "transpose sexp")
    "ks" '(sp-forward-slurp-sexp :which-key "slurp forward")
    "kS" '(sp-backward-slurp-sexp :which-key "slurp backward")
    "kb" '(sp-forward-barf-sexp :which-key "barf forward")
    "kB" '(sp-backward-barf-sexp :which-key "barf backward"))
  
  (evil-define-key '(normal visual) 'global
    (kbd "M-k") #'memacs/sp-move-sexp-up
    (kbd "M-j") #'memacs/sp-move-sexp-down))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure))
  :config
  (memacs/minor-leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
    "'" '(sesman-start :which-key "start sesman")
    "e" '(:ignore t :which-key "evaluation")
    "r" '(:ignore t :which-key "refactor")
    "s" '(:ignore t :which-key "send to repl")
    "sq" '(:ignore t :which-key "quit/restart repl")
    "eb" '(cider-load-buffer :which-key "eval buffer")
    "ef" '(cider-eval-defun-at-point :which-key "eval defun")
    "el" '(cider-eval-list-at-point :which-key "eval list")
    "es" '(cider-eval-sexp-at-point :which-key "eval sexp")
    "rr" '(eglot-rename :which-key "rename")
    "sqq" '(cider-quit :which-key "quit cider")))

(use-package cider
  :defer t
  :custom
  (cider-download-java-sources t)
  (cider-repl-pop-to-buffer-on-connect nil))

(use-package cider-storm
  :after cider
  :vc (:url "https://github.com/flow-storm/cider-storm" :rev :newest)
  :config
  (memacs/minor-leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map)
    "d" '(:ignore t :which-key "debug")
    "df" '(cider-storm-debug-fn :which-key "function")
    "dt" '(cider-storm-toggle-recording :which-key "toggle debugger")))

(provide 'setup-clojure)
