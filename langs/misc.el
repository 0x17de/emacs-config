(use-package emr)
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :ensure t
  :demand
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (expand-file-name "_0x17de" user-emacs-directory)))
  (yas-global-mode 1)
  :bind (("C-M-S-y" . 'yas-describe-tables)))

(use-package x509-mode)
(use-package yaml-mode)
(use-package dot-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package json-mode)
(use-package elf-mode)
(use-package demangle-mode)
(use-package systemd)
(use-package easy-hugo)
(use-package gh-md)
(use-package magit)

(use-package ein)
(use-package ess-mode
  :ensure ess
  :config
  (add-hook 'ess-mode-hook
            (lambda ()
              ('company-mode t)))
  (define-key ess-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key ess-mode-map [(f1)] 'company-show-doc-buffer))

;; reuse compilation buffer from other frames
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*" . (display-buffer-reuse-window
                          . ((reusable-frames . t)))))
