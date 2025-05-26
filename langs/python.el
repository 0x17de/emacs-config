(use-package realgud
  :defer t)
(defcustom _0x17de/python-isort-on-save nil
  "Enable automatic import sorting with py-isort on save.
When non-nil, py-isort-buffer will be called before saving Python files."
  :type 'boolean
  :group '_0x17de
  :safe #'booleanp)
(use-package python
  :ensure nil
  :defer t
  :bind (:map python-mode-map
              ([tab] . company-indent-or-complete-common)
              ([f1] . lsp-describe-thing-at-point)
              ([f12] . lsp-find-definition)
              ("S-<f12>" . lsp-find-references)
              ;; Imports
              ("C-c i s" . py-isort-buffer)
              ;; Formatting
              ("C-c f b" . python-black-buffer)
              ("C-c f r" . python-black-region)
              ("C-c f a" . py-autopep8-buffer)
              ;; REPL
              ("C-c C-p" . run-python)
              ("C-c C-r" . python-shell-send-region)
              ("C-c C-b" . python-shell-send-buffer)
              ("C-c C-l" . python-shell-send-file)
              ;; Documentation
              ("C-c h h" . python-eldoc-at-point)
              ("C-c h d" . python-describe-at-point)
              ("C-c h f" . python-info-lookup-symbol)
              ;; Navigation
              ("M-." . xref-find-definitions)
              ("M-," . xref-pop-marker-stack)
              ("M-?" . xref-find-references)
              ("C-c r" . lsp-find-references)
              ("C-M-." . lsp-find-implementation)
              ("C-M-," . lsp-find-type-definition)
              ("C-c n" . python-nav-forward-defun)
              ("C-c p" . python-nav-backward-defun)
              ;; Errors
              ("C-c e n" . flycheck-next-error)
              ("C-c e p" . flycheck-previous-error)
              ("C-c e l" . flycheck-list-errors)
              ("C-c e c" . flycheck-clear)
              ("C-c e v" . flycheck-verify-setup))
  :hook ((python-mode . (lambda ()
                          (setq yas-indent-line 'fixed)
                          (company-mode t)
                          (flycheck-mode t)
                          (highlight-indent-guides-mode t)
                          (rainbow-delimiters-mode t)
                          (when _0x17de/python-isort-on-save
                            (add-hook 'before-save-hook 'py-isort-buffer nil t))
                          (lsp-deferred)))))
(use-package py-isort
  :defer t
  :commands (py-isort-buffer py-isort-region)
  :config
  (setq py-isort-options '("--line-length=100" "--multi-line=3")))
(use-package blacken
  :defer t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length 100
        blacken-skip-string-normalization t))
(use-package python-docstring
  :defer t
  :hook (python-mode . python-docstring-mode)
  :config
  (setq python-docstring-sentence-end-double-space nil))
(use-package coverage
  :defer t
  :commands (coverage-mode coverage-clear-overlays)
  :bind (:map python-mode-map
              ("C-c t c" . coverage-mode)
              ("C-c t C" . coverage-clear-overlays)))
(use-package poetry
  :defer t
  :commands (poetry-venv-activate poetry-install poetry-add)
  :config
  (setq poetry-tracking-strategy 'switch-buffer))
(use-package lsp-pyright
  :defer t
  :after lsp-mode
  :config
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-multi-root t
        lsp-pyright-exclude ["**/.mypy_cache"
                             "**/__pycache__"
                             "**/node_modules"
                             ".git"]))

(defcustom _0x17de/python-global-virtualenv-dir "~/.venv"
  "Default directory for Python virtual environments.
This is used by pyvenv to locate and activate virtual environments."
  :type 'directory
  :group '_0x17de
  :safe #'stringp)
(use-package pyvenv
  :defer t
  :after python
  :hook (python-mode . pyvenv-mode)
  :bind (:map python-mode-map
              ("C-c v a" . pyvenv-activate)
              ("C-c v d" . pyvenv-deactivate)
              ("C-c v w" . pyvenv-workon))
  :init
  (setq pyvenv-workon-home _0x17de/python-global-virtualenv-dir)
  :config
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (lsp-restart-workspace))))
  (add-hook 'pyvenv-post-deactivate-hooks
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (lsp-restart-workspace)))))
