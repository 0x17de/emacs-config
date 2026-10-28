;; -*- lexical-binding: t; -*-
(use-package lsp-java
  :defer t
  :hook (java-mode . (lambda ()
                       (setq c-basic-offset 4
                             lsp-java-completion-enabled t
                             lsp-java-completion-overwrite nil
                             lsp-java-completion-guess-method-arguments t
                             lsp-enable-completion-at-point t)
                       (flycheck-mode t)
                       (lsp-deferred))))
