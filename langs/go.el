;; -*- lexical-binding: t; -*-
(use-package go-guru
  :defer t)
(use-package go-eldoc
  :defer t)
(use-package godoctor
  :defer t)
(use-package go-scratch
  :defer t)
(use-package go-playground
  :defer t)
(use-package flycheck-golangci-lint
  :defer t)
(use-package go-complete
  :defer t)

(defun ox-install-go-dependencies ()
  "Install go-mode dependencies"
  (interactive)
  (let ((commands '("go install golang.org/x/tools/cmd/goimports@latest"
                    "go install github.com/godoctor/godoctor@latest"
                    "go install golang.org/x/tools/cmd/guru@latest"
                    "go install github.com/rogpeppe/godef@latest"
                    "go install golang.org/x/tools/cmd/godoc@latest"
                    "go install github.com/zmb3/gogetdoc@latest"
                    "go install golang.org/x/tools/gopls@latest")))
    (dolist (cmd commands)
      (message "Running: %s" cmd)
      (shell-command cmd)
      (message "Completed: %s" cmd))
    (message "All Go dependencies installed!")))
(use-package go-mode
  :defer t
  :bind
  (:map go-mode-map
        ([tab] . 'company-indent-or-complete-common)
        ([f1] . lsp-describe-thing-at-point)
        ([f12] . lsp-find-definition)
        ("S-<f12>" . lsp-find-references))
  :hook
  (go-mode . (lambda ()
               (setq tab-width 2)
               (setq company-backends '(company-capf
                                        company-files))
               (setq gofmt-command "goimports")
               (add-hook 'before-save-hook 'gofmt-before-save)
               (go-guru-hl-identifier-mode)
               (lsp-deferred)
               (setenv "GOGC" "20")
               (setenv "GOMEMLIMIT" "4GiB")
               (setq lsp-gopls-staticcheck nil)
               (setq lsp-eldoc-enable-hover nil)
               (setq lsp-eldoc-render-all nil)
               (setq lsp-gopls-complete-unimported t)
               (setq lsp-gopls-use-placeholders t)
               (setq lsp-gopls-codelens nil)
               (setq lsp-idle-delay 0.5)
               (setq lsp-file-watch-threshold 2000)
               (setq lsp-gopls-workspace-folders-filter 'filesystem-root)
               (company-mode t)))
  :config
  (setq go-godoc-reuse-buffer t)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  (setq godoc-use-completing-read t))
