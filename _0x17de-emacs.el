;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;;
;; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
;; (load "_0x17de-emacs")

(setq debug-on-error t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(global-unset-key (kbd "C-z")) ; stop me from freezing emacs

;; Tab fix
(global-set-key (kbd "<backtab>") 'insert-tab-char)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
;; Tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Auto indentation
(add-to-list 'load-path "guess-style")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)


(ido-mode t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package multi-term
  :ensure t
  :config
  (load "multi-term-settings.el"))
(use-package multiple-cursors
  :ensure t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-M-z C-c" . mc/edit-lines)
   ("C-M-z >" . mc/mark-next-like-this)
   ("C-M-z <" . mc/mark-previous-like-this)
   ("C-M-z |" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))
;;(use-package sudo-edit)
(use-package refine
  :ensure t)
(use-package x509-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package dot-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package docker-compose-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package elf-mode
  :ensure t)
(use-package demangle-mode
  :ensure t)
(use-package systemd
  :ensure t)
(use-package easy-hugo
  :ensure t)
(use-package gh-md
  :ensure t)
(use-package magit
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)

(use-package function-args
  :ensure t)
(use-package helm-swoop
  :ensure t)
(use-package helm-gtags
  :ensure t)
(use-package counsel
  :ensure t)

;; see https://github.com/company-mode/company-mode/issues/525#issuecomment-348635719
(defun inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    (message "%s" result)
    result))
(defun inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (let ((result (nth 4 (syntax-ppss))))
    (message "%s" result)
    result))
(defun semantic-completion-advice (adviced-f &rest r)
  "Check if POINT it's inside a string or comment before calling semantic-*"
  (unless (or (inside-string-q) (inside-comment-q))
    (apply adviced-f r)))

(use-package semantic
  :config
  (advice-add 'semantic-analyze-completion-at-point-function
              :around #'semantic-completion-advice)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (semantic-mode 1))
(use-package emr
  :ensure t)
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-M-x") 'execute-extended-command))
(use-package srefactor
  :ensure t)
(use-package irony
  :ensure t)
;(use-package auto-complete
;  :init
;  (setq tab-always-indent 'complete)
;  (setq ac-disable-inline t)
;  :config
;  (setq ac-sources (delete 'ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(use-package auctex
;  :ensure t)
;; (use-package auto-complete-auctex)
(use-package company-quickhelp
  :ensure t)
(defun company-init-quickhelp (b-enable)
  "Initialize company-quickhelp only if company is running"
  (company-quickhelp-mode t)
  (remove-hook 'company-completion-started-hook 'company-init-quickhelp t))
(use-package company
  :ensure t
  :init 
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.5)
  (setq company-async-timeout 5)
  :config
  (add-hook 'company-completion-started-hook 'company-init-quickhelp)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-capf company-backends)))
(use-package company-lsp
  :ensure t)
(use-package company-jedi
  :ensure t)
(use-package company-irony
  :ensure t)
(use-package company-c-headers
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package flycheck-irony
  :ensure t)
(use-package flycheck-rust
  :ensure t)
(use-package cmake-ide
  :ensure t
  :config
  ;; override provided function to rather use the "build"
  ;; directory in the projects root than a temp directory
  (defun cide--build-dir-var ()
    "Use cmake-ide-build-dir, cmake-ide-dir or the build directory inside the project root"
    (or cmake-ide-build-dir
        cmake-ide-dir
        (concat (cide--locate-project-dir) "build")))
  (cmake-ide-setup))
(use-package cmake-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("CMakeInstallTargets\\.txt\\'" . cmake-mode)))
(use-package cpputils-cmake
  :ensure t
  :config
  (add-hook 'cmake-mode-hook 'company-mode)
  (define-key cmake-mode-map [(tab)] 'company-indent-or-complete-common))

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-lsp)
              (company-mode t)))
  (define-key lisp-mode-map [(tab)] 'company-indent-or-complete-common))
(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-lsp)
              (company-mode t)))
  (define-key emacs-lisp-mode-map [(tab)] 'company-indent-or-complete-common))
;; go-lang support
;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/godoctor/godoctor
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/guru
;; go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
;; go get github.com/rogpeppe/godef
;; go get golang.org/x/tools/cmd/godoc
;; go get github.com/zmb3/gogetdoc
;; go get -u github.com/golang/dep/cmd/dep
(use-package go-guru
  :ensure t)
(use-package godoctor
  :ensure t)
(use-package go-scratch
  :ensure t)
(use-package go-playground
  :ensure t)
(use-package flycheck-golangci-lint
  :ensure t)
(use-package company-go
  :ensure t)
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-go)
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save)
              (go-guru-hl-identifier-mode)
              (flycheck-golangci-lint-setup)
              (flycheck-mode t)
              (company-mode t)))
  (setq go-godoc-reuse-buffer t)
  (setq godoc-at-point-function 'godoc-gogetdoc)
  (setq godoc-command "godoc")
  (setq godoc-use-completing-read t)
  (define-key go-mode-map [(tab)] 'company-indent-or-complete-common))
(use-package cc-mode
  :config
  (load "ext/google-styleguide/google-c-style")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-irony-c-headers)
              (add-to-list 'company-backends 'company-c-headers)
              (add-to-list 'company-backends 'company-irony)
              (company-mode t)
              (irony-mode t)
              (cmake-ide-maybe-run-cmake)
              (hs-minor-mode t)
              (flycheck-mode t)
              (rainbow-delimiters-mode t)
              (google-set-c-style)))
  (define-key c-mode-map [(f1)] 'semantic-ia-show-doc)
  (define-key c++-mode-map [(f1)] 'semantic-ia-show-doc)
  (define-key c-mode-map [(f5)] 'recompile)
  (define-key c++-mode-map [(f5)] 'recompile)
  (define-key c-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key c++-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key c-mode-map (kbd "C-C C-j") 'moo-jump-local)
  (define-key c++-mode-map (kbd "C-C C-j") 'moo-jump-local)
  (define-key c-mode-map (kbd "C-C M-j") 'semantic-ia-fast-jump)
  (define-key c++-mode-map (kbd "C-C M-j") 'semantic-ia-fast-jump))
(use-package auto-virtualenv
  :ensure t)
(use-package jedi
  :ensure t)
(use-package realgud
  :ensure t)
(use-package python
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-jedi)
              (auto-virtualenv-set-virtualenv)
              (company-mode t)
              (flycheck-mode t)
              (hs-minor-mode t)))
  (define-key python-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key python-mode-map [(f1)] 'jedi:show-doc))

(defun my/rust-setup ()
  "Setup the rust variables; Environment variables overwrite internal variables"
  (unless (getenv "RUST_SRC_PATH")
    (let ((rust-source-dir (expand-file-name "~/src/rust/src")))
      (setenv "RUST_SRC_PATH" rust-source-dir)
      (setq racer-rust-src-path rust-source-dir)))
  (unless racer-rust-src-path
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))))

(use-package racer
  :ensure t)
(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-racer)
              (my/rust-setup)
              (racer-mode t)
              (eldoc-mode t)
              (company-mode t)
              (flycheck-mode t)
              (flycheck-rust-setup)))
  (define-key rust-mode-map [(f1)] 'racer-describe)
  (define-key rust-mode-map [(f5)] 'rust-compile)
  (define-key rust-mode-map [(tab)] 'company-indent-or-complete-common))
(use-package org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c .") 'org-time-stamp))))
(load "ext/ox-confluence/ox-confluence")
(use-package ein
  :ensure t)
(use-package ess-mode
  :ensure ess
  :config
  (add-hook 'ess-mode-hook
            (lambda ()
              ('company-mode t)))
  (define-key ess-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key ess-mode-map [(f1)] 'company-show-doc-buffer))

(use-package projectile
  :ensure t
  :config
  ;; (projectile-global-mode)
  (setq projectile-enable-caching t))

(defun my/latex-setup ()
  "Setup the latex environment"
  (reftex-mode t)
  (auctex-latexmk-setup)
  (TeX-engine-set "luatex")
  (setq font-latex-fontify-script 'multi-level)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (TeX-fold-mode 1)
  (rainbow-delimiters-mode t)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-auctex)
  (company-auctex-init)
  (company-mode t))
(defun my/latexmk-run ()
  "Run latexmk on the master file"
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error)
      (minibuffer-message "latexmk-done"))))
(use-package auctex-latexmk
  :ensure t)
(use-package company-auctex
  :ensure t)
(use-package latex
  :config
  (add-hook 'TeX-mode-hook 'my/latex-setup)
  (define-key TeX-mode-map [(f5)] 'my/latexmk-run)
  (define-key TeX-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key TeX-mode-map [(f1)] 'company-show-doc-buffer))

(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'company-meghanada)
              (meghanada-mode t)
              (company-mode t)))
  (define-key java-mode-map [(tab)] 'company-indent-or-complete-common))

;;(use-package zygospore
;;             :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
;;                    ("RET" . newline-and-indent)))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (concat (file-name-directory load-file-name) "snippets"))
  (yas-global-mode 1)
  :bind (("C-M-S-y" . 'yas-describe-tables)))

;;(global-set-key (kbd "C-c w") 'whitespace-mode)
;;(windmove-default-keybindings)

;; notes: speedbar, sr-speedbar

(load "latex-compile-on-save.el")
(load "ext/misc/dired+")

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
(load "ext/misc/vline")
(load "ext/misc/col-highlight")
(load "ext/misc/crosshairs")

;; reuse compilation buffer from other frames
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*" . (display-buffer-reuse-window
                          . ((reusable-frames . t)))))

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "<C-M-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-M-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-M-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-M-S-right>")  'buf-move-right))

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key (kbd "C-M-S-q") 'kill-this-buffer)
(global-set-key (kbd "C-M-S-x") 'multi-term)
(global-set-key (kbd "C-M-S-w") (lambda () (interactive)
                                  (kill-this-buffer)
                                  (if (equal 1 (length (window-list)))
                                      (delete-frame)
                                    (delete-window))))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-M-<up>") (lambda () (interactive)
                                   (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 5))))
(global-set-key (kbd "C-M-<down>") (lambda () (interactive)
                                     (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 5))))
;;(global-set-key (kbd "C-M-<up>") 'text-scale-increase)
;;(global-set-key (kbd "C-M-<down>") 'text-scale-decrease)
(global-set-key (kbd "C-M-z C-e") 'eval-region)
(global-set-key (kbd "C-M-z C-M-e") 'eval-buffer)
(global-set-key (kbd "C-M-S-c") 'find-emacs-config)
(global-set-key (kbd "C-M-S-z") 'cmake-ide-compile)
(defun find-emacs-config ()
  "Open the emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/_0x17de/_0x17de-emacs.el"))

;; Git diff fix
(setq vc-handled-backends ())

;; Autocomplete changes
(setq icomplete-mode t)
;; (setq completion-cycle-threshold t)"
(setq completion-auto-help t)

;; Selection/Clipboard improvements
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)
;;focus follow mouse
(setq mouse-autoselect-window 0
      focus-follows-mouse t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; See https://github.com/ch11ng/exwm/wiki
;; https://github.com/ch11ng/exwm/wiki/Configuration-Example
(when (boundp 'use-exwm)
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)
  ;;(ido-mode 1)
  (require 'exwm)
  (require 'exwm-config)
  ;;(exwm-config-ido)
  ;;(exwm-config-default)
  (require 'exwm-randr)
  ;;(add-hook 'exwm-randr-screen-change-hook
  ;;        (lambda ()
  ;;          (start-process-shell-command
  ;;           "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  (require 'exwm-systemtray)
  ;;(setq exwm-workspace-number 4)
  ;;(setq mouse-autoselect-window nil)
  (global-set-key [mode-line mouse-2] 'exwm-layout-toggle-fullscreen)
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          [s-o] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock"))))
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
  (exwm-enable)
  (exwm-randr-enable)
  (exwm-systemtray-enable))



;;No splash screen
(setq inhibit-startup-message t)

;;Disable bars
(add-hook 'after-make-frame-functions 'simplify-ui)
(defun simplify-ui (frame)
  (interactive)
  "Remove unwanted menubar/scrollbar/etc"
  (modify-frame-parameters frame
                           '((menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fringe-mode 1)
  (display-time-mode t))

;;Diff adjustements
(setq ediff-split-window-function 'split-window-horizontally)
;;/etc/etc-update.conf
;;diff_command="emacs-diff %file1 %file2"
;;using_editor=1
;;merge_command="emacs-merge %orig %new %merged"

;;Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)


(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


;;Mouse scroll
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;;Term or GUI mode mouse changes
(unless (display-graphic-p)
  (require 'mouse)
  ;;http://stackoverflow.com/questions/3466643/emacs-unicode-xterm-mouse-escape-sequences-and-wide-terminals
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


(setq compile-command "ewcompile")
(defun my-compile ()
  "Compile"
  (interactive)
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
    (insert "\n#endif")
    (jump-to-register)))


;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (make-local-variable 'indent-tabs-mode)
	     (setq indent-tabs-mode nil)
	     (add-to-list 'rng-schema-locating-files
			  "~/.emacs.d/nxml-schemas/schemas.xml")))


(load "directory-helper-functions")

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(customize-set-variable 'custom-enabled-themes '(sanityinc-tomorrow-bright))
(customize-set-variable 'show-paren-mode t)

(customize-set-variable 'ansi-color-faces-vector [default default default italic underline success warning error])
(customize-set-variable 'ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
(customize-set-variable 'custom-safe-themes (quote ("cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
(customize-set-variable 'dired-mode-hook (quote (dired-hide-details-mode)))
(customize-set-variable 'show-paren-mode t)
;;(customize-set-faces '(default ((t (:height 102 :width semi-condensed))))
;; '(col-highlight ((t (:inherit highlight))))
;; '(vline ((t (:background "dim gray")))))
(put 'downcase-region 'disabled nil)
