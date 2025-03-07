(use-package irony
  :config
  (require 'irony-cdb)
  (customize-set-variable 'irony-cdb-compilation-databases '(irony-cdb-json
                                                             irony-cdb-clang-complete
                                                             irony-cdb-libclang)))

(use-package company-irony)
(use-package company-c-headers)
(use-package flycheck-irony)

(use-package cmake-mode
  :config
  (add-to-list 'auto-mode-alist '("CMakeInstallTargets\\.txt\\'" . cmake-mode)))
(use-package cpputils-cmake
  :config
  (add-hook 'cmake-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (setq company-backends '(company-cmake
                                       company-files))
              (company-mode t)))
  (define-key cmake-mode-map [(f1)] 'cmake-help)
  (define-key cmake-mode-map [(f5)] 'recompile)
  (define-key cmake-mode-map [(tab)] 'company-indent-or-complete-common))

(defun cross-recompile ()
  "Recompile using cross-compile-command"
  (interactive)
  (let ((compile-command cross-compile-command))
    (recompile)))
(defun c-mode-common-init ()
  "Callback for initialization of c-like modes"
  (make-local-variable 'company-backends)
  (setq company-backends '((company-irony-c-headers company-c-headers)
                           company-irony
                           company-files))
  (company-mode t)
  (irony-mode t)
  (irony-eldoc t)
  (irony-cdb-autosetup-compile-options)
  (hs-minor-mode t)
  (flycheck-mode t)
  (flycheck-irony-setup)
  (rainbow-delimiters-mode t)
  (google-set-c-style)
  (setq c-basic-offset 4)
  )
(use-package cc-mode
  :ensure nil
  :config
  (load "ext/google-styleguide/google-c-style")
  (add-hook 'c-mode-hook 'c-mode-common-init)
  (add-hook 'c++-mode-hook 'c-mode-common-init)
  (define-key c-mode-map [(f1)] 'semantic-ia-show-doc)
  (define-key c++-mode-map [(f1)] 'semantic-ia-show-doc)
  (define-key c-mode-map [(f5)] 'recompile)
  (define-key c-mode-map [(f6)] 'cross-recompile)
  (define-key c-mode-map [(f7)] 'srefactor-refactor-at-point)
  (define-key c-mode-map [(f8)] 'oxci--run-cmake)
  (define-key c++-mode-map [(f5)] 'recompile)
  (define-key c++-mode-map [(f6)] 'cross-recompile)
  (define-key c++-mode-map [(f7)] 'srefactor-refactor-at-point)
  (define-key c++-mode-map [(f8)] 'oxci--run-cmake)
  (define-key c-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key c++-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key c-mode-map (kbd "C-C C-j") 'moo-jump-local)
  (define-key c++-mode-map (kbd "C-C C-j") 'moo-jump-local)
  (define-key c-mode-map (kbd "C-C M-j") 'semantic-ia-fast-jump)
  (define-key c++-mode-map (kbd "C-C M-j") 'semantic-ia-fast-jump)
  (load "ox-cmake-ide.el")
  (ox-cmake-ide))

(setq compile-command "ewcompile")
(setq cross-compile-command "ewcompile") ; override via .dir-locals.el
(defun my-compile ()
  "Compile"
  (interactive)
  (setq compilation-search-path (or (cide--build-dir) default-directory))
  (compile compile-command))
(global-set-key (kbd "C-M-z C-M-a") 'my-compile)
(global-set-key (kbd "C-M-z C-a") 'my-compile)

(defun defguard (guard)
  "Inserts guard header for C++"
  (interactive "sGuard name: ")
  (save-excursion
    (beginning-of-buffer)
    (insert (concat "#ifndef " (upcase guard) "_H\n#define " (upcase guard) "_H\n\n"))
    (end-of-buffer)
    (insert "\n#endif")))
