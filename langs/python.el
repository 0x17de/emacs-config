;; -*- lexical-binding: t; -*-
(use-package realgud
  :defer t)
(defcustom _0x17de/python-lsp-ruff-enable nil
  "When non-nil, enable ruff LSP server alongside pyright for live linting.
Requires lsp-ruff package and `ruff server' in PATH (ruff >= 0.2.0)."
  :type 'boolean
  :group '_0x17de
  :safe #'booleanp)
(defun _0x17de/python--setup-keys (map)
  "Define shared Python keybindings in MAP."
  (define-key map [tab]           #'company-indent-or-complete-common)
  (define-key map [f1]            #'lsp-describe-thing-at-point)
  (define-key map [f12]           #'lsp-find-definition)
  (define-key map (kbd "S-<f12>") #'lsp-find-references)
  (define-key map (kbd "C-c i s") #'ruff-isort-buffer)
  (define-key map (kbd "C-c C-p") #'run-python)
  (define-key map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key map (kbd "C-c C-b") #'python-shell-send-buffer)
  (define-key map (kbd "C-c C-l") #'python-shell-send-file)
  (define-key map (kbd "C-c h h") #'python-eldoc-at-point)
  (define-key map (kbd "C-c h d") #'python-describe-at-point)
  (define-key map (kbd "C-c h f") #'python-info-lookup-symbol)
  (define-key map (kbd "M-.")     #'xref-find-definitions)
  (define-key map (kbd "M-,")     #'xref-go-back)
  (define-key map (kbd "M-?")     #'xref-find-references)
  (define-key map (kbd "C-c r")   #'lsp-find-references)
  (define-key map (kbd "C-M-.")   #'lsp-find-implementation)
  (define-key map (kbd "C-M-,")   #'lsp-find-type-definition)
  (define-key map (kbd "C-c n")   #'python-nav-forward-defun)
  (define-key map (kbd "C-c p")   #'python-nav-backward-defun)
  (define-key map (kbd "C-c e n") #'flycheck-next-error)
  (define-key map (kbd "C-c e p") #'flycheck-previous-error)
  (define-key map (kbd "C-c e l") #'flycheck-list-errors)
  (define-key map (kbd "C-c e c") #'flycheck-clear)
  (define-key map (kbd "C-c e v") #'flycheck-verify-setup)
  (define-key map (kbd "C-c f f") #'ruff-format-buffer))
(defun _0x17de/python--setup ()
  "Common setup shared between python-mode and python-ts-mode."
  (setq yas-indent-line 'fixed)
  (company-mode t)
  (flycheck-mode t)
  (highlight-indent-guides-mode t)
  (rainbow-delimiters-mode t)
  (_0x17de/python--setup-keys (current-local-map)))
(use-package python
  :ensure nil
  :defer t
  :hook ((python-mode . _0x17de/python--setup)
         (python-ts-mode . _0x17de/python--setup)))
(use-package python-docstring
  :defer t
  :hook ((python-mode . python-docstring-mode)
         (python-ts-mode . python-docstring-mode))
  :config
  (setq python-docstring-sentence-end-double-space nil))
(use-package coverage
  :defer t
  :commands (coverage-mode coverage-clear-overlays)
  :bind (:map python-mode-map
              ("C-c t c" . coverage-mode)
              ("C-c t C" . coverage-clear-overlays)
         :map python-ts-mode-map
              ("C-c t c" . coverage-mode)
              ("C-c t C" . coverage-clear-overlays)))
(use-package lsp-pyright
  :defer t
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
         (python-ts-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp-deferred))))
  :config
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-multi-root nil
        lsp-pyright-exclude ["**/.mypy_cache"
                             "**/__pycache__"
                             "**/node_modules"
                             ".git"]))

(when _0x17de/python-lsp-ruff-enable
  (use-package lsp-ruff
    :defer t
    :hook ((python-mode . (lambda () (require 'lsp-ruff) (lsp-deferred)))
           (python-ts-mode . (lambda () (require 'lsp-ruff) (lsp-deferred))))))

(use-package reformatter
  :demand t
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args '("format" "-"))
  (reformatter-define ruff-isort
    :program "ruff"
    :args '("check" "--select" "I" "--fix" "--fix-only" "-")))
;; isort added first so it is prepended before format in python-mode-hook,
;; meaning format ends up at the front and runs last — correct order on save.
(add-hook 'python-mode-hook #'ruff-isort-on-save-mode)
(add-hook 'python-mode-hook #'ruff-format-on-save-mode)
(add-hook 'python-ts-mode-hook #'ruff-isort-on-save-mode)
(add-hook 'python-ts-mode-hook #'ruff-format-on-save-mode)
