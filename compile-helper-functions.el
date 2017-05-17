;;Compile functions
(defun compile-cmake ()
  "Compile a project in the tree"
  (interactive)
  (let* ((default-directory (or (upward-find-dir "build") "."))
         (compile-command (concat "cd " default-directory "/build && " compile-command)))
    (compile compile-command)))

(defun compile-make ()
  "Compile a project in the tree"
  (interactive)
  (let* ((default-directory (or (upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && " compile-command)))
    (compile compile-command)))

(defun compile-policy ()
  (interactive)
  (let* ((compile-command (concat compile-command " && semodule -i " (replace-regexp-in-string "\.\\(te\\|if\\|fc\\)$" ".pp" buffer-file-name))))
    (compile compile-command)))
