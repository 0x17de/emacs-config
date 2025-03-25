(use-package flycheck-rust
  :defer t)
;(use-package racer)
(use-package rust-mode
  :defer t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              ;(racer-mode t)
              (eldoc-mode t)
              (company-mode t)
              (flycheck-mode t)
              (flycheck-rust-setup)
              (lsp)))
  (define-key rust-mode-map [(f5)] 'rust-compile)
  (define-key rust-mode-map [(tab)] 'company-indent-or-complete-common))
(setq lsp-rust-analyzer-server-command '("rust-analyzer"))
