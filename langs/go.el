(use-package go-guru)
(use-package go-eldoc)
(use-package godoctor)
(use-package go-scratch)
(use-package go-playground)
(use-package flycheck-golangci-lint)
;;(use-package company-go)
(use-package go-complete)

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
  :ensure-system-package
  ((goimports     . "go install golang.org/x/tools/cmd/goimports@latest")
   (godoc         . "go install github.com/godoctor/godoctor@latest")
   ;;(gocode        . "go install github.com/mdempsky/gocode@latest")
   (guru          . "go install golang.org/x/tools/cmd/guru@latest")
   ;;broken: (golangci-lint . "go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest")
   (godef         . "go install github.com/rogpeppe/godef@latest")
   (godoc         . "go install golang.org/x/tools/cmd/godoc@latest")
   (gogetdoc      . "go install github.com/zmb3/gogetdoc@latest")
   ;(dep           . "go install github.com/golang/dep/cmd/dep@latest")
   (gopls         . "go install golang.org/x/tools/gopls@latest"))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq company-backends '(company-capf
                                       company-files))
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save)
              (go-guru-hl-identifier-mode)
              (lsp-deferred)
              (setenv "GOGC" "20")
              (setenv "GOMEMLIMIT" "4GiB")
              ;(setq lsp-gopls-server-args '("-logfile=gopls.log" "-v"))
              (setq lsp-gopls-staticcheck nil)
              (setq lsp-eldoc-enable-hover nil)
              (setq lsp-eldoc-render-all nil)
              (setq lsp-gopls-complete-unimported t)
              (setq lsp-gopls-use-placeholders t)
              (setq lsp-gopls-codelens nil)
              (setq lsp-idle-delay 0.5)
              (setq lsp-file-watch-threshold 2000)
              (setq lsp-gopls-workspace-folders-filter 'filesystem-root)
              ;(setq lsp-log-io t)
              (company-mode t)))
  (setq go-godoc-reuse-buffer t)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  (setq godoc-use-completing-read t)
  (define-key go-mode-map [(tab)] 'company-indent-or-complete-common))
