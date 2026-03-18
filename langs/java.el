;; -*- lexical-binding: t; -*-
(use-package lsp-java
  :defer t
  :hook (java-mode . (lambda ()
                       (setq c-basic-offset 4
                             lsp-java-completion-enabled t
                             lsp-java-completion-overwrite nil
                             lsp-java-completion-guess-method-arguments t
                             lsp-enable-completion-at-point t
                             lsp-completion-provider :capf
                             company-backends '(company-capf company-files))
                       (company-mode t)
                       (flycheck-mode t)
                       (lsp-deferred)))
  :bind (:map java-mode-map
              ([tab] . #'company-indent-or-complete-common)))
