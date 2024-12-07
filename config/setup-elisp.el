;; Idea: go to end of list, call `eros-eval-last-sexp'
(defun my-eval-list-at-point ()
  "Evaluate the top-level list at point, similar to `cider-eval-list-at-point` in Clojure.
Displays the result in the minibuffer."
  (interactive)
  ;; Ensure we're in a relevant mode
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-excursion
      ;; Check if we're inside a list
      (condition-case nil
          (backward-up-list) ;; Move to the beginning of the list
        (scan-error (error "Not inside a list")))
      ;; Get the boundaries of the list
      (let ((start (point)))
        ;; Move to the end
        (forward-list)
        (let ((end (point)))
          ;; Evaluate the region and capture the result
          (let ((result (eval (read (buffer-substring-no-properties start end)))))
            ;; Display the result in the minibuffer
            (message "Result: %s" result)))))))

(defun memacs-align-lisp-comments ()
  "Align comments marked with ';'."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\);"))

(use-package eros
  :config
  (my-leader-def
    :keymaps 'emacs-lisp-mode-map
    "a" #'memacs-align-lisp-comments
    "eb" 'eval-buffer
    "ef" 'eros-eval-defun
    "el" 'my-eval-list-at-point
    "es" 'eros-eval-last-sexp)
  (which-key-add-key-based-replacements
    ", e" "evaluation")
  (eros-mode 1))

(provide 'setup-elisp)
