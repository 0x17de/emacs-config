;; -*- lexical-binding: t; -*-
(defcustom _0x17de/google-c-style-overrides
  '(((c-basic-offset) 4)
    ((c-offsets-alist member-init-intro) ++)
    ((c-offsets-alist access-label) -))
  "Overwrite individual keys in google-c-style.

Each element of this list has the form:

  ((KEY1 KEY2 ...) NEW-VALUE)

- The first part is a list of keys that describes how to traverse
  into the `google-c-style` structure. If there's only a single key,
  it modifies that top-level attribute. If multiple keys are given,
  they are traversed in sequence to locate the nested attribute.

- The second part is the NEW-VALUE to set for that attribute."
  :group '_0x17de
  :type '(repeat (list (repeat symbol) sexp)))
(defun _0x17de/google-c-style-set-overrides ()
  "Apply the overrides in `_0x17de/google-c-style-overrides' to `google-c-style' in place."
  (interactive)
  (dolist (cfg _0x17de/google-c-style-overrides)
    (let* ((init google-c-style))
      (dolist (c (car cfg))
        (setq init (assoc c init)))
      ;; cadr, not cdr: the value is the second element, and `(cdr cfg)' would
      ;; store the enclosing list, e.g. c-basic-offset -> (4) instead of 4.
      (setf (cdr init) (cadr cfg)))))

(use-package cmake-mode
  :defer t
  :mode (("CMakeInstallTargets\\.txt\\'" . cmake-mode))
  :bind (:map cmake-mode-map
         ([f1] . cmake-help)
         ([f5] . recompile)))

(defun _0x17de/c-mode-common-init ()
  "Callback for initialization of c-like modes"
  (hs-minor-mode t)
  (flycheck-mode t)
  (rainbow-delimiters-mode t)
  (condition-case err
      (google-set-c-style)
    (error (message "Failed to set google-c-style: Did loading it fail? %S" err)))
  (setq-local c-basic-offset 4)
  ;; clangd, via lsp-mode's built-in client. Needs a compile_commands.json in
  ;; the project (CMAKE_EXPORT_COMPILE_COMMANDS=ON) to resolve includes.
  ;; Guarded so that a machine without clang-tools still gets completion,
  ;; flycheck and the style setup instead of an lsp server prompt.
  (if (executable-find "clangd")
      (lsp-deferred)
    (message "clangd not found in PATH; C/C++ LSP features disabled")))
(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (condition-case err
      (progn
        (_0x17de/load "ext/google-styleguide/google-c-style")
        (_0x17de/google-c-style-set-overrides))
    (error (message "Failed to load google-c-style. Did you also sync the git submodules? %S" err)))
  ;; Do not quote these: use-package reads a quoted cdr as a list of hook
  ;; names, which silently adds `cc-mode' to c-mode-hook instead.
  :hook ((c-mode . _0x17de/c-mode-common-init)
         (c++-mode . _0x17de/c-mode-common-init))
  :bind (:map c-mode-map
              ([f5] . recompile)
         :map c++-mode-map
              ([f5] . recompile)))

(defun defguard (guard)
  "Inserts guard header for C++"
  (interactive "sGuard name: ")
  (save-excursion
    (beginning-of-buffer)
    (insert (concat "#ifndef " (upcase guard) "_H\n#define " (upcase guard) "_H\n\n"))
    (end-of-buffer)
    (insert "\n#endif")))
