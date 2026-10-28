;; -*- lexical-binding: t; -*-
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
              (flycheck-mode t)
              (flycheck-rust-setup)
              (lsp)))
  (define-key rust-mode-map [(f5)] 'rust-compile))
(setq lsp-rust-analyzer-server-command '("rust-analyzer"))
