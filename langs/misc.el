;; -*- lexical-binding: t; -*-
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
  :init
  (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)
         ("<f5>" . projectile-compile-project)
         ("S-<f5>" . projectile-test-project)
         ("<f6>" . projectile-run-project))
  :custom
  (projectile-project-search-path '("~/git" "~/src"))
  (projectile-enable-caching t))

(use-package yasnippet
  :demand t
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
    (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" _0x17de/load-path))
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
  :defer t
  :custom
  (magit-bury-buffer-function 'magit-restore-window-configuration))

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
