(use-package emr
  :defer t)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (salt-mode . rainbow-delimiters-mode)))
(use-package rainbow-mode
  :config
  (progn (setq rainbow-x-colors nil)
         (add-hook 'prog-mode-hook 'rainbow-mode)))

(use-package projectile
  :demand t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yasnippet
  :demand t
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
    (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (expand-file-name "_0x17de" user-emacs-directory)))
    (yas-global-mode 1))
  :bind (("C-M-S-y" . 'yas-describe-tables)))

(use-package x509-mode
  :defer t)
(use-package yaml-mode
  :defer t)
(use-package dot-mode
  :defer t)
(use-package dockerfile-mode
  :defer t)
(use-package docker-compose-mode
  :defer t)
(use-package json-mode
  :defer t)
(use-package elf-mode
  :defer t)
(use-package demangle-mode
  :defer t)
(use-package systemd
  :defer t)
(use-package easy-hugo
  :defer t)
(use-package gh-md
  :defer t)
(use-package magit
  :defer t)

(use-package ein
  :defer t)
(use-package ess-mode
  :ensure ess
  :defer t
  :hook (ess-mode . (lambda ()
                      ('company-mode t)))
  :bind (:map ess-mode-map
              ([tab] . company-indent-or-complete-common)
              ([f1] . company-show-doc-buffer))
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*compilation\\*" . (display-buffer-reuse-window
                            . ((reusable-frames . t))))))
