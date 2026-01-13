(defun memacs/eval-list-at-point ()
  "Evaluate the top-level list at point and display results inline and in minibuffer."
  (interactive)
  (save-excursion
    (condition-case nil
        (backward-up-list)
      (scan-error (error "Not inside a list")))
    (let* ((start (point))
           (end (progn (forward-list) (point)))
           (result (eval (read (buffer-substring-no-properties start end))))
           (print-length eval-expression-print-length)
           (print-level eval-expression-print-level))
      (eros--eval-overlay result end)
      (prin1 result t)
      (let ((str (eval-expression-print-format result)))
        (when str (princ str t)))
      result)))

(defun memacs/align-lisp-comments ()
  "Align comments marked with ';'."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\);"))

(use-package eros
  :config
  (memacs/minor-leader-def
    :keymaps 'emacs-lisp-mode-map
    "e"  '(:ignore t :which-key "evaluation")
    "a"  '(memacs/align-lisp-comments :which-key "align lisp comments")
    "eb" '(eval-buffer :which-key "eval buffer")
    "ef" '(eros-eval-defun :which-key "eval defun")
    "el" '(memacs/eval-list-at-point :which-key "eval list")
    "es" '(eros-eval-last-sexp :which-key "eval last sexp"))
  (eros-mode 1))

(provide 'setup-elisp)
