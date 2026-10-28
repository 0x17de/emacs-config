;; -*- lexical-binding: t; -*-
(use-package go-scratch
  :defer t)
(use-package go-playground
  :defer t)
(use-package flycheck-golangci-lint
  :defer t)

(defun ox-install-go-dependencies ()
  "Install go-mode dependencies"
  (interactive)
  ;; gopls covers what guru, godef, gogetdoc and godoctor used to do.
  (let ((commands '("go install golang.org/x/tools/cmd/goimports@latest"
                    "go install golang.org/x/tools/gopls@latest")))
    (dolist (cmd commands)
      (message "Running: %s" cmd)
      (shell-command cmd)
      (message "Completed: %s" cmd))
    (message "All Go dependencies installed!")))
(defun _0x17de/go--setup ()
  "Common setup shared between go-mode and go-ts-mode."
  (setq-local tab-width 2)
  ;; Consulted per buffer at render time, so keep these local
  ;; instead of flipping eldoc off for every other language.
  (setq-local lsp-eldoc-enable-hover nil)
  (setq-local lsp-eldoc-render-all nil)
  ;; goimports on save; see apheleia in langs/common.el.
  (apheleia-mode t)
  (lsp-deferred))
(use-package go-mode
  :defer t
  :hook ((go-mode . _0x17de/go--setup)
         (go-ts-mode . _0x17de/go--setup)))
;; Global, but gopls-specific. After lsp-go rather than go-mode, which never
;; loads when go-ts-mode is used. These used to run per buffer, which meant
;; e.g. lsp-file-watch-threshold was permanently lowered for every language
;; as soon as a Go file was opened; it now keeps the value set in
;; langs/common.el.
(with-eval-after-load 'lsp-go
  (setenv "GOGC" "20")
  (setenv "GOMEMLIMIT" "4GiB")
  (setq lsp-gopls-staticcheck nil
        lsp-gopls-complete-unimported t
        lsp-gopls-use-placeholders t
        lsp-gopls-codelens nil
        lsp-gopls-workspace-folders-filter 'filesystem-root))
