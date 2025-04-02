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
      (setf (cdr init) (cdr cfg)))))

(use-package cmake-mode
  :defer t
  :mode (("CMakeInstallTargets\\.txt\\'" . cmake-mode))
  :bind (:map cmake-mode-map
         ([f1] . cmake-help)
         ([f5] . recompile)
         ([tab] . company-indent-or-complete-common))
  :hook (cmake-mode . (lambda ()
                        (company-mode t))))

(defun _0x17de/c-mode-common-init ()
  "Callback for initialization of c-like modes"
  (company-mode t)
  (hs-minor-mode t)
  (flycheck-mode t)
  (flycheck-irony-setup)
  (rainbow-delimiters-mode t)
  (condition-case err
      (google-set-c-style)
    (error (message "Failed to set google-c-style: Did loading it fail? %S" err)))
  (setq c-basic-offset 4))
(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (condition-case err
      (progn
        (load "ext/google-styleguide/google-c-style")
        (_0x17de/google-c-style-set-overrides))
    (error (message "Failed to load google-c-style. Did you also sync the git submodules? %S" err)))
  :hook ((c-mode . '_0x17de/c-mode-common-init)
         (c++-mode . '_0x17de/c-mode-common-init))
  :bind (:map c-mode-map
              ([f1] . semantic-ia-show-doc)
              ([f5] . recompile)
              ([f6] . srefactor-refactor-at-point)
              ([tab] . company-indent-or-complete-common)
         :map c++-mode-map
              ([f1] . semantic-ia-show-doc)
              ([f5] . recompile)
              ([f6] . srefactor-refactor-at-point))
              ([tab] . company-indent-or-complete-common))

(defun defguard (guard)
  "Inserts guard header for C++"
  (interactive "sGuard name: ")
  (save-excursion
    (beginning-of-buffer)
    (insert (concat "#ifndef " (upcase guard) "_H\n#define " (upcase guard) "_H\n\n"))
    (end-of-buffer)
    (insert "\n#endif")))
