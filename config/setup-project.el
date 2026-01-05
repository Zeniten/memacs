;;; setup-project.el --- Project navigation and management utilities

(defun memacs/--parse-file-info (file-path)
  "Parse FILE-PATH into structured file information.
Returns plist with :file :dir :filename :base-name :extension
:project-root :relative-path. Returns nil if file-path is invalid."
  (when (and file-path (file-name-absolute-p file-path))
    (let* ((filename (file-name-nondirectory file-path))
           (dir (file-name-directory file-path))
           (extension (file-name-extension filename))
           (base-name (file-name-sans-extension filename))
           (project (project-current))
           (project-root (when project (project-root project)))
           (relative-path (when project-root
                           (file-relative-name file-path project-root))))
      (when (> (length base-name) 0)
        (list :file file-path
              :dir dir
              :filename filename
              :base-name base-name
              :extension extension
              :project-root project-root
              :relative-path relative-path)))))

(defun memacs/--is-test-file-p (base-name)
  "Return implementation base name if BASE-NAME represents a test file.
Returns the implementation base name (without test suffix) if true, nil otherwise."
  (when (string-match "\\(.*\\)[-_]test$" base-name)
    (let ((impl-base (match-string 1 base-name)))
      (and (> (length impl-base) 0) impl-base))))

(defun memacs/--build-impl-candidates (file-info impl-base)
  "Build list of implementation file candidates from FILE-INFO plist and IMPL-BASE.
Returns list ordered by priority: same-dir first, then test->src mappings."
  (let* ((extension (plist-get file-info :extension))
         (dir (plist-get file-info :dir))
         (project-root (plist-get file-info :project-root))
         (relative-path (plist-get file-info :relative-path))
         (impl-name (concat impl-base "." extension))
         (candidates '()))

    ;; Priority 1: Same directory
    (setq candidates
          (append candidates
                  (list (expand-file-name impl-name dir))))

    ;; Priority 2: test/tests/ -> src/ with subdirectory preservation
    (when (and project-root
               relative-path
               (string-match "^\\(tests?\\)/\\(.+\\)" relative-path))
      (let ((subdir (match-string 2 relative-path)))
        (setq candidates
              (append candidates
                      (list (expand-file-name
                             (concat "src/" (file-name-directory subdir) impl-name)
                             project-root))))))

    ;; Priority 3: Parent directory (if in test/tests/ subdirectory)
    (when (and relative-path
               (string-match "\\(tests?\\)/" relative-path))
      (setq candidates
            (append candidates
                    (list (expand-file-name impl-name (expand-file-name ".." dir))))))

    candidates))

(defun memacs/--build-test-candidates (file-info)
  "Build list of test file candidates from FILE-INFO plist.
Returns list ordered by priority: same-dir first, then src->test mappings."
  (let* ((base-name (plist-get file-info :base-name))
         (extension (plist-get file-info :extension))
         (dir (plist-get file-info :dir))
         (project-root (plist-get file-info :project-root))
         (relative-path (plist-get file-info :relative-path))
         (test-patterns '("-test" "_test"))
         (test-dirs '("test" "tests"))
         (candidates '()))

    ;; Priority 1: Same directory (both patterns)
    (dolist (pattern test-patterns)
      (setq candidates
            (append candidates
                    (list (expand-file-name
                           (format "%s%s.%s" base-name pattern extension)
                           dir)))))

    ;; Priority 2: src/ -> test/tests/ with subdirectory preservation
    (when (and project-root
               relative-path
               (string-match "^src/\\(.+\\)" relative-path))
      (let ((subdir (match-string 1 relative-path)))
        (dolist (test-dir test-dirs)
          (dolist (pattern test-patterns)
            (setq candidates
                  (append candidates
                          (list (expand-file-name
                                 (format "%s/%s%s%s.%s"
                                         test-dir
                                         (file-name-directory subdir)
                                         base-name
                                         pattern
                                         extension)
                                 project-root))))))))

    ;; Priority 3: Simple test/ and tests/ at project root
    (when project-root
      (dolist (test-dir test-dirs)
        (dolist (pattern test-patterns)
          (setq candidates
                (append candidates
                        (list (expand-file-name
                               (format "%s/%s%s.%s" test-dir base-name pattern extension)
                               project-root)))))))

    candidates))

(defun memacs/project-toggle-implementation-test ()
  "Toggle between implementation and test files in a project.
Handles common test patterns with directory structure preservation:
- foo.el <-> foo-test.el (Emacs Lisp, same directory)
- foo.clj <-> foo_test.clj (Clojure, same directory)
- src/foo/bar.clj <-> test/foo/bar_test.clj (preserves subdirectories)"
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))

  (let* ((file-info (memacs/--parse-file-info buffer-file-name))
         (base-name (plist-get file-info :base-name))
         (filename (plist-get file-info :filename)))

    (unless (and file-info base-name)
      (user-error "Cannot parse file name: %s" buffer-file-name))

    (let* ((impl-base (memacs/--is-test-file-p base-name))
           (candidates (if impl-base
                          (memacs/--build-impl-candidates file-info impl-base)
                        (memacs/--build-test-candidates file-info)))
           (target-file (seq-find #'file-exists-p candidates)))

      (if target-file
          (find-file target-file)
        (message "No %s file found for %s"
                (if impl-base "implementation" "test")
                filename)))))

(provide 'setup-project)
