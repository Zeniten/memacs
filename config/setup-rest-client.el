(use-package json-navigator
  :defer t)

(use-package restclient
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :custom
  (restclient-response-size-threshold 500000)
  :bind (:map restclient-response-buffer-map
         ;; Override 'q' to always close the response window properly.
         ;; Without this, after using json-navigator, 'q' would switch to the
         ;; source buffer instead of closing the response window.
         ("q" . delete-window))
  :config
  (setf (alist-get "application/json" restclient-content-type-modes nil nil #'equal)
        'json-ts-mode))

(provide 'setup-rest-client)
