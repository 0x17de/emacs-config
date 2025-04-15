(use-package realgud
  :defer t)
(use-package python
  :ensure nil
  :defer t
  :bind (:map python-mode-map
              ([tab] . company-indent-or-complete-common)
              ([f1] . lsp-describe-thing-at-point)
              ([f12] . lsp-find-definition)
              ("S-<f12>" . lsp-find-references))
  :hook ((python-mode . (lambda ()
                          (setq yas-indent-line 'fixed)
                          (company-mode t)
                          (flycheck-mode t)
                          (hs-minor-mode t)
                          (highlight-indent-guides-mode t)
                          (rainbow-delimiters-mode t)
                          (lsp-deferred)))))
(use-package pyimport
  :defer t
  :commands (pyimport-insert-missing pyimport-remove-unused))
(use-package pyimpsort
  :defer t
  :commands pyimpsort-buffer)
(use-package lsp-pyright
  :defer t
  :after lsp-mode
  :config
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-multi-root t
        lsp-pyright-exclude ["**/node_modules" ".git" "**/__pycache__"
                             "**/tests" "**/test" "**/.mypy_cache"]))

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
