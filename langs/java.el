;; -*- lexical-binding: t; -*-
(use-package lsp-java
  :defer t
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (setq c-basic-offset 4)
              (setq lsp-java-completion-enabled t)
              (setq lsp-java-completion-overwrite nil)
              (setq lsp-java-completion-guess-method-arguments t)
              (setq lsp-enable-completion-at-point t)
              (setq lsp-completion-provider :capf)
              (company-mode t)
              (flycheck-mode t)
              (setq company-backends '(company-capf company-files))
              (lsp-deferred)))
  (define-key java-mode-map (kbd "TAB") 'company-indent-or-complete-common))
