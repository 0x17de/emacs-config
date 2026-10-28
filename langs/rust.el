;; -*- lexical-binding: t; -*-
(use-package flycheck-rust
  :defer t)
(defun _0x17de/rust--setup ()
  "Common setup shared between rust-mode and rust-ts-mode."
  (eldoc-mode t)
  (flycheck-mode t)
  (flycheck-rust-setup)
  (lsp-deferred))
(use-package rust-mode
  :defer t
  ;; On the hooks rather than in :config: with rust-ts-mode, rust-mode
  ;; never loads.
  :hook ((rust-mode . _0x17de/rust--setup)
         (rust-ts-mode . _0x17de/rust--setup))
  :bind (:map rust-mode-map
              ([f5] . rust-compile)))
(setq lsp-rust-analyzer-server-command '("rust-analyzer"))
