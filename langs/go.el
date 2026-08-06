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
               (setq-local tab-width 2)
               (setq-local company-backends '(company-capf
                                              company-files))
               (setq-local gofmt-command "goimports")
               ;; Consulted per buffer at render time, so keep these local
               ;; instead of flipping eldoc off for every other language.
               (setq-local lsp-eldoc-enable-hover nil)
               (setq-local lsp-eldoc-render-all nil)
               (add-hook 'before-save-hook #'gofmt-before-save nil t)
               (go-guru-hl-identifier-mode)
               (lsp-deferred)
               (company-mode t)))
  :config
  ;; Global, but gopls-specific and only relevant once go-mode is loaded.
  ;; These used to run per buffer, which meant e.g. lsp-file-watch-threshold
  ;; was permanently lowered for every language as soon as a Go file was
  ;; opened; it now keeps the value set in langs/common.el.
  (setenv "GOGC" "20")
  (setenv "GOMEMLIMIT" "4GiB")
  (setq lsp-gopls-staticcheck nil
        lsp-gopls-complete-unimported t
        lsp-gopls-use-placeholders t
        lsp-gopls-codelens nil
        lsp-gopls-workspace-folders-filter 'filesystem-root)
  (setq go-godoc-reuse-buffer t)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  (setq godoc-use-completing-read t))
