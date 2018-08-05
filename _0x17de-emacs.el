;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;
; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
; (load "_0x17de-emacs")

;;http://ergoemacs.org/emacs/emacs_package_system.html
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))



;;initial setup of recommended packages i use
(defvar dotemacs-packages
  ; doremi doremi-cmd doremi-frm 
  '(multiple-cursors cmake-mode color-theme-sanityinc-tomorrow
                     multi-term
                     flycheck flycheck-irony
                     cmake-mode yaml-mode
                     demangle-mode elf-mode
                     dockerfile-mode docker-compose-mode
                     dot-mode
                     easy-hugo
                     company company-lsp
                     company-irony company-irony-c-headers company-rtags cmake-ide)
  "All packages i require")
(defvar
  dotemacs-needs-install nil)
(dolist (p dotemacs-packages)
  (message "Checking if %s is installed: %s" p (package-installed-p p))
  (when (not (package-installed-p p))
    (setq dotemacs-needs-install t)))
(when dotemacs-needs-install
  (message "Installing missing packages...")
  (package-refresh-contents)
  ;; install missing packages
  (dolist (p dotemacs-packages)
    (when (not (package-installed-p p))
      (message "Installing: %s" p)
      (package-install p))))

(setq multi-term-program "/bin/zsh")
(defun term-handle-exit--close-buffer (&rest args)
  (when (null (get-buffer-process (current-buffer)))
    (insert "Press <C-d> to kill the buffer.")
    (use-local-map (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "C-d")
                       (lambda ()
                         (interactive)
                         (kill-buffer (current-buffer))))
                     map))))
(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer)

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
(load "ext/misc/vline")
(load "ext/misc/col-highlight")
(load "ext/misc/crosshairs")



;;No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key (kbd "C-M-S-q") 'kill-this-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-M-<up>") 'text-scale-increase)
(global-set-key (kbd "C-M-<down>") 'text-scale-decrease)
(global-set-key (kbd "C-M-z C-e") 'eval-region)
(global-set-key (kbd "C-M-z C-M-e") 'eval-buffer)
(global-set-key (kbd "C-M-S-c") 'find-emacs-config)
(defun find-emacs-config ()
  "Open the emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/_0x17de/_0x17de-emacs.el"))

;;Git diff fix
(setq vc-handled-backends ())

;;Autocomplete changes
(setq icomplete-mode t)
;(setq completion-cycle-threshold t)"
(setq completion-auto-help t)

;;Selection/Clipboard improvements
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)



;;Tab fix
(global-set-key (kbd "<backtab>") 'insert-tab-char)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
;;Tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;Auto indentation
(add-to-list 'load-path "guess-style")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)



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
  (menu-bar-mode -1))

;;Diff adjustements
(setq ediff-split-window-function 'split-window-horizontally)
;;/etc/etc-update.conf
;diff_command="emacs-diff %file1 %file2"
;using_editor=1
;merge_command="emacs-merge %orig %new %merged"

;;Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)



(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)



;;Multi cursor edit
(when (require 'multiple-cursors nil 'noerror)
      (global-set-key (kbd "C-M-z C-c") 'mc/edit-lines)
      (global-set-key (kbd "C-M-z >") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-M-z <") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-M-z |") 'mc/mark-all-like-this)
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

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



(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'lisp-mode-hook 'company-mode)

(load "ext/google-styleguide/google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; autocompletion and highlighting modules
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-rtags company-irony-c-headers company-irony company-lsp)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

; TODO:
;   M-x rtags-install
;   M-x irony-install-server

; rtags navigation
(require 'rtags)
(require 'flycheck-rtags)
(require 'company-rtags)
(require 'company-irony-c-headers)
(setq rtags-completions-enabled t)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'company-mode)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

; company+irony autocompletion
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(cmake-ide-setup)


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



(load "directory-helper-functions")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(dired-mode-hook (quote (dired-hide-details-mode)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 102 :width semi-condensed)))))


