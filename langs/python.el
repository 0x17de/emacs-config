(use-package auto-virtualenv)
(use-package realgud)
(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (make-local-variable 'yas-indent-line)
              (setq company-backends '(company-jedi
                                       company-files))
              (setq yas-indent-line 'fixed)
              (company-mode t)
              (flycheck-mode t)
              (hs-minor-mode t)
              (highlight-indent-guides-mode t)
              (rainbow-delimiters-mode t)
              (lsp-deferred)))
  (define-key python-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key python-mode-map [(f1)] 'lsp-describe-thing-at-point))
(use-package pyimport)
(use-package pyimpsort)
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'fill)
  (set-face-background 'highlight-indent-guides-odd-face "gray18")
  (set-face-background 'highlight-indent-guides-even-face "gray20")
  (set-face-foreground 'highlight-indent-guides-character-face "gray18"))
(use-package lsp-pyright
  :after lsp-mode
  :config
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t))
