;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;;
;; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
;; (load "_0x17de-emacs")

;(setq debug-on-error t)
(setq initial-scratch-message nil)

(load "init-custom.el")
(load "init-speedup.el")
(load "init-package.el")
(load "init-encoding.el")
(load "init-gui.el")

(global-unset-key (kbd "C-z")) ; stop me from freezing emacs
;; Always group items in buffer menu
(setq mouse-buffer-menu-mode-mult 0)
;; Use ibuffer instead of temporary buffer popup
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
;(global-set-key (kbd "<home>") 'smart-beginning-of-line)

;; Tab fix
(global-set-key (kbd "<backtab>") 'insert-tab-char)
(global-set-key (kbd "<C-tab>") 'company-complete)
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

(defun replace-with-shell (posBegin posEnd command)
  "Pipe region through shell command and replace"
  (interactive (let (command)
		 (unless (mark)
		   (user-error "The mark is not set now, so there is no region"))
                 (setq command (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end) command)))
  (let ((string (buffer-substring-no-properties posBegin posEnd))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert string)
      (shell-command-on-region (point-min) (point-max) command (current-buffer))
      (setq string (buffer-substring-no-properties (point-min) (point-max))))
    (delete-region posBegin posEnd)
    (insert string)))

(global-set-key (kbd "C-M-|") 'replace-with-shell)


(ido-mode t)
;(use-package color-theme-sanityinc-tomorrow
;  :ensure t)
(use-package smart-mode-line
  :config
  (sml/setup))
(use-package multi-term
  :config
  (load "multi-term-settings.el"))
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-M-z C-c" . mc/edit-lines)
   ("C-M-z >" . mc/mark-next-like-this)
   ("C-M-z <" . mc/mark-previous-like-this)
   ("C-M-z |" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))
;;(use-package sudo-edit)
(use-package refine)
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
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package function-args)
(use-package helm-swoop)
(use-package helm-gtags)
(use-package counsel)

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

(use-package dumb-jump
  :init
  (global-set-key (kbd "C-S-j") 'dumb-jump-go))
(use-package semantic
  :config
  (advice-add 'semantic-analyze-completion-at-point-function
              :around #'semantic-completion-advice)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-idle-completions-mode -1)
  (semantic-mode 1))
(use-package stickyfunc-enhance)
(use-package emr)
(use-package smex
  :init
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-M-x") 'execute-extended-command))
(use-package srefactor)
(use-package irony
  :config
  (require 'irony-cdb)
  (customize-set-variable 'irony-cdb-compilation-databases '(irony-cdb-json
                                                             irony-cdb-clang-complete
                                                             irony-cdb-libclang)))
;(use-package auto-complete
;  :init
;  (setq tab-always-indent 'complete)
;  (setq ac-disable-inline t)
;  :config
;  (setq ac-sources (delete 'ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(use-package auctex
;  :ensure t)
;; (use-package auto-complete-auctex)
(use-package company-quickhelp)
(defun company-init-quickhelp (b-enable)
  "Initialize company-quickhelp only if company is running"
  (company-quickhelp-mode t)
  (remove-hook 'company-completion-started-hook 'company-init-quickhelp t))
(use-package company
  :init 
  (setq company-async-timeout 5
        company-dabbrev-downcase 0
        company-idle-delay 0.5
        company-irony-ignore-case t
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  :config
  (add-hook 'company-completion-started-hook 'company-init-quickhelp)
  (setq company-backends '())) ; we explicitly add all the backends we need
(use-package company-lsp)
(use-package company-jedi)
(use-package company-irony)
(use-package company-c-headers)
(use-package flycheck)
(use-package flycheck-irony)
(use-package flycheck-rust)
;(use-package cmake-ide
;  :after (company company-c-headers irony flycheck)
;  :config
;  ;; override provided function to rather use the "build"
;  ;; directory in the projects root than a temp directory
;  (defun cide--build-dir-var ()
;    "Use cmake-ide-build-dir, cmake-ide-dir or the build directory inside the project root"
;    (or cmake-ide-build-dir
;        cmake-ide-dir
;        (concat (cide--locate-project-dir) "build")))
                                        ;  (cmake-ide-setup))
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

;(use-package lsp-ui)

(require 'lisp-mode)
(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'company-backends)
            (setq company-backends '(company-lsp
                                     company-files))
            (company-mode t)))
(define-key lisp-mode-map [(tab)] 'company-indent-or-complete-common)

(require 'elisp-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'company-backends)
            (setq company-backends '(company-elisp
                                     company-lsp
                                     company-files))
            (company-mode t)))
(define-key emacs-lisp-mode-map [(tab)] 'company-indent-or-complete-common)

(use-package go-guru)
(use-package go-eldoc)
(use-package godoctor)
(use-package go-scratch)
(use-package go-playground)
(use-package flycheck-golangci-lint)
(use-package company-go)
(use-package go-mode
  :ensure-system-package
  ((goimports     . "go get -u golang.org/x/tools/cmd/goimports")
   (godoc         . "go get -u github.com/godoctor/godoctor")
   (gocode        . "go get -u github.com/nsf/gocode")
   (guru          . "go get -u golang.org/x/tools/cmd/guru")
   (golangci-lint . "go get -u github.com/golangci/golangci-lint/cmd/golangci-lint")
   (godef         . "go get -u github.com/rogpeppe/godef")
   (godoc         . "go get -u golang.org/x/tools/cmd/godoc")
   (gogetdoc      . "go get -u github.com/zmb3/gogetdoc")
   (dep           . "go get -u github.com/golang/dep/cmd/dep"))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (make-local-variable 'company-backends)
              (setq company-backends '(company-go
                                       company-files))
              (setq gofmt-command "goimports")
              (add-hook 'go-mode-hook 'go-eldoc-setup)
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
(use-package auto-virtualenv)
;(use-package jedi
;  :ensure t)
(use-package realgud)
(use-package python
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (make-local-variable 'yas-indent-line)
              (setq company-backends '(company-jedi
                                       company-files))
              (setq yas-indent-line 'fixed)
              (auto-virtualenv-set-virtualenv)
              (company-mode t)
              (flycheck-mode t)
              (hs-minor-mode t)
	      (highlight-indent-guides-mode t)
	      (rainbow-delimiters-mode t)))
  (define-key python-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key python-mode-map [(f1)] 'jedi:show-doc))
(use-package pyimport)
(use-package pyimpsort)
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'fill)
  (set-face-background 'highlight-indent-guides-odd-face "gray18")
  (set-face-background 'highlight-indent-guides-even-face "gray20")
  (set-face-foreground 'highlight-indent-guides-character-face "gray18"))

(defun my/rust-setup ()
  "Setup the rust variables; Environment variables overwrite internal variables"
  (unless (getenv "RUST_SRC_PATH")
    (let ((rust-source-dir (expand-file-name "~/src/rust/src")))
      (setenv "RUST_SRC_PATH" rust-source-dir)
      (setq racer-rust-src-path rust-source-dir)))
  (unless racer-rust-src-path
    (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))))

(use-package racer)
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (setq company-backends '(company-racer
                                       company-files))
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
(use-package ein)
(use-package ess-mode
  :ensure ess
  :config
  (add-hook 'ess-mode-hook
            (lambda ()
              ('company-mode t)))
  (define-key ess-mode-map [(tab)] 'company-indent-or-complete-common)
  (define-key ess-mode-map [(f1)] 'company-show-doc-buffer))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (setq company-backends '(company-auctex
                           company-files))
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
(use-package auctex-latexmk)
(use-package company-auctex)

(require 'latex)
(add-hook 'TeX-mode-hook 'my/latex-setup)
(define-key TeX-mode-map [(f5)] 'my/latexmk-run)
(define-key TeX-mode-map [(tab)] 'company-indent-or-complete-common)
(define-key TeX-mode-map [(f1)] 'company-show-doc-buffer)

(use-package meghanada
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (setq company-backends '(company-meghanada
                                       company-files))
              (setq c-basic-offset 4)
              (meghanada-mode t)
              (flycheck-mode t)
              (company-mode t)))
  (define-key java-mode-map [(tab)] 'company-indent-or-complete-common))

;;(use-package zygospore
;;             :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
;;                    ("RET" . newline-and-indent)))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (file-name-directory load-file-name)))
  (yas-global-mode t)
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

(load "init-exwm.el")

;;Diff adjustements
(setq ediff-split-window-function 'split-window-horizontally)
;;/etc/etc-update.conf
;;diff_command="emacs-diff %file1 %file2"
;;using_editor=1
;;merge_command="emacs-merge %orig %new %merged"

(global-font-lock-mode t)
(delete-selection-mode t)
(auto-compression-mode t)
(line-number-mode t)
(column-number-mode t)
;;Show matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0)
(transient-mark-mode t)
(global-hl-line-mode t)
;(setq cursor-type 'bar)
(setq redisplay-dont-pause t)
;(setq ring-bell-function 'ignore)

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
;(customize-set-variable 'dired-mode-hook (quote (dired-hide-details-mode)))
;(customize-set-variable 'show-paren-mode t)
;(customize-set-variable 'custom-enabled-themes '(gruvbox))
;(customize-set-variable 'ansi-color-faces-vector [default default default italic underline success warning error])
;(customize-set-variable 'ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
(put 'downcase-region 'disabled nil)
