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

;; (foo (fisk test naa))
;; (if (= 1 3)
;;     "foo"
;;   "bar")

(use-package smartparens
  :defer t
  :hook (prog-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (which-key-add-key-based-replacements
    "<SPC> k" "lisp")
  (evil-define-key '(normal visual) 'global
    (kbd "M-k") #'memacs/sp-move-sexp-up
    (kbd "M-j") #'memacs/sp-move-sexp-down
    (kbd "<SPC>kw") #'sp-wrap-round
    (kbd "<SPC>k(") #'sp-wrap-round
    (kbd "<SPC>k)") #'sp-wrap-round
    (kbd "<SPC>k[") #'sp-wrap-square
    (kbd "<SPC>k]") #'sp-wrap-square
    (kbd "<SPC>k{") #'sp-wrap-curly
    (kbd "<SPC>k}") #'sp-wrap-curly
    (kbd "<SPC>kr") #'sp-raise-sexp
    (kbd "<SPC>kt") #'sp-transpose-sexp
    (kbd "<SPC>ks") #'sp-forward-slurp-sexp
    (kbd "<SPC>kS") #'sp-backward-slurp-sexp
    (kbd "<SPC>kb") #'sp-forward-barf-sexp
    (kbd "<SPC>kB") #'sp-backward-barf-sexp))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure))
  :config
  (memacs/leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
    "'" #'sesman-start
    "eb" #'cider-load-buffer
    "ef" #'cider-eval-defun-at-point
    "el" #'cider-eval-list-at-point
    "es" #'cider-eval-sexp-at-point
    "rr" #'eglot-rename
    "sqq" #'cider-quit)
  (which-key-add-key-based-replacements
    ", e" "evaluation"
    ", r" "refactor"
    ", s" "send to repl"
    ", sq" "quit/restart repl"
    ))

(use-package cider
  :defer t
  :custom
  (cider-download-java-sources t)
  (cider-repl-pop-to-buffer-on-connect nil))

(use-package cider-storm
  :after cider
  :vc (:url "https://github.com/flow-storm/cider-storm" :rev :newest))

(provide 'setup-clojure)
