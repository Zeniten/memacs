(require 'setup-converters)

(defvar memacs/clojure-mode-maps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
  "List of Clojure-related mode keymaps.")

(defun memacs/clojure-find-references ()
  "Find references using Eglot (clojure-lsp) for complete indexing.
Forces Eglot's backend by temporarily overriding xref-backend-functions."
  (interactive)
  (if (eglot-managed-p)
      ;; Force Eglot backend by setting it as the only option
      (let ((xref-backend-functions '(eglot-xref-backend t)))
        (call-interactively #'xref-find-references))
    ;; Fallback to normal xref if Eglot not available
    (call-interactively #'xref-find-references)))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :hook (((clojure-mode clojurescript-mode clojurec-mode) . eglot-ensure)
         ((clojure-mode clojurescript-mode clojurec-mode) . (lambda ()
                                                               (evil-local-set-key 'normal "gr" #'memacs/clojure-find-references))))
  :custom
  (clojure-toplevel-inside-comment-form t)
  :config

  ;; https://clojure.org/guides/weird_characters#_discard
  (defun memacs/discard-next-form ()
    "Discard the next Clojure form, i.e., prepending #_."
    (interactive)
    (save-excursion
      (clojure-forward-logical-sexp)
      (backward-sexp)
      (insert "#_")))

  (memacs/minor-leader-def
    :keymaps memacs/clojure-mode-maps
    "'" '(sesman-start :which-key "start sesman")
    "c" '(:ignore t :which-key "convert")
    "e" '(:ignore t :which-key "evaluation")

    "h" '(:ignore t :which-key "help")
    "ha" '(cider-apropos :which-key "apropos")
    "hc" '(cider-cheatsheet :which-key "cheatsheet")
    "hd" '(cider-doc :which-key "doc")
    "hj" '(cider-javadoc :which-key "javadoc")
    "hn" '(cider-browse-ns :which-key "browse ns")
    "hN" '(cider-browse-ns-all :which-key "browse all ns")
    "hs" '(cider-browse-spec :which-key "browse spec")
    "hS" '(cider-browse-spec-all :which-key "browse all specs")

    "i" '(:ignore t :which-key "insertion")
    "r" '(:ignore t :which-key "refactor")
    "s" '(:ignore t :which-key "send to repl")
    "t" '(:ignore t :which-key "test")
    "sq" '(:ignore t :which-key "quit/restart repl")
    "ch" '(html-to-hiccup-convert-region :which-key "html->hiccup")
    "eb" '(cider-load-buffer :which-key "eval buffer")
    "ef" '(cider-eval-defun-at-point :which-key "eval defun")
    "el" '(cider-eval-list-at-point :which-key "eval list")
    "es" '(cider-eval-sexp-at-point :which-key "eval sexp")
    "id" '(memacs/discard-next-form :which-key "discard form")
    "rr" '(eglot-rename :which-key "rename")
    "ta" '(cider-test-run-project-tests :which-key "all")
    "tf" '(cider-test-rerun-failed-tests :which-key "failed")
    "tn" '(cider-test-run-ns-tests :which-key "namespace")
    "tr" '(cider-test-rerun-failed-tests :which-key "rerun")
    "ts" '(cider-test-show-report :which-key "show report")
    "tt" '(cider-test-run-test :which-key "test")
    "sqq" '(cider-quit :which-key "quit cider")

    "v" '(:ignore t :which-key "view")
    "vl" #'cider-inspect-last-result))

(defun memacs/prefer-eglot-over-cider ()
  "Remove CIDER completion when both CIDER and Eglot are active."
  (when (and (bound-and-true-p cider-mode)
             (bound-and-true-p eglot--managed-mode))
    ;; TODO Do you really want to remove CIDER completion?
    ;; Maybe CIDER is superior for everything except reference lookup?
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point 'local)
    (setq-local eldoc-documentation-functions
		(delq 'cider-eldoc eldoc-documentation-functions))))

(use-package cider
  :defer t
  :hook
  ((cider-mode . memacs/prefer-eglot-over-cider)
   (eglot-managed-mode . memacs/prefer-eglot-over-cider))
  :custom
  (cider-download-java-sources t)
  (cider-repl-pop-to-buffer-on-connect nil)

  ;; Save files before loading into REPL
  ;; Note: "load" (cider-load-buffer) saves the file first, then loads it into the REPL.
  ;; "eval" functions (cider-eval-*) evaluate the current buffer state without saving.
  (cider-save-file-on-load t)

  ;; re-use dead buffers without asking me about it when there is only one choice
  (cider-reuse-dead-repls 'auto)

  ;; Prefer Eglot over CIDER when both are available, but allow CIDER as fallback:
  (cider-eldoc-display-for-symbol-at-point nil) ; Use Eglot's eldoc
  (cider-completion-use-context nil)            ; Use Eglot's completion (via hook)

  :config
  (evil-set-initial-state 'cider-inspector-mode 'emacs))

(use-package cider-storm
  :after cider
  :vc (:url "https://github.com/flow-storm/cider-storm" :rev :newest)
  :config
  (memacs/minor-leader-def
    :keymaps memacs/clojure-mode-maps
    "d" '(:ignore t :which-key "debug")
    "df" '(cider-storm-debug-fn :which-key "function")
    "ds" '(cider-storm-storm-start-gui :which-key "start")
    "dt" '(cider-storm-toggle-recording :which-key "toggle debugger")))

(provide 'setup-clojure)
