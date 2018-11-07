;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;
; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
; (load "_0x17de-emacs")

(setq debug-on-error t)

;;http://ergoemacs.org/emacs/emacs_package_system.html
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
; '("marmalade" . "http://marmalade-repo.org/packages/")



;;initial setup of recommended packages i use
(defvar dotemacs-packages
  ; doremi doremi-cmd doremi-frm 
  '(multiple-cursors cmake-mode color-theme-sanityinc-tomorrow
                     multi-term sudo-edit buffer-move refine
                     treemacs x509-mode
                     flycheck flycheck-irony
                     cmake-mode yaml-mode jedi
                     demangle-mode elf-mode
                     dockerfile-mode docker-compose-mode
                     dot-mode json-mode yaml-mode
                     systemd
                     easy-hugo
                     magit magit-gh-pulls magithub
                     company company-lsp company-jedi
                     company-irony company-irony-c-headers cmake-ide)
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

(global-unset-key (kbd "C-z")) ; stop me from freezing emacs

(load "magit.el")
(load "multi-term-settings.el")

(load "ext/misc/dired+")

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
(load "ext/misc/vline")
(load "ext/misc/col-highlight")
(load "ext/misc/crosshairs")




(require 'yasnippet)
(yas-global-mode 1)


(add-to-list 'auto-mode-alist '("CMakeInstallTargets\\.txt\\'" . cmake-mode))
; reuse compilation buffer from other frames
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*" . (display-buffer-reuse-window
                          . ((reusable-frames . t)))))



(require 'buffer-move)
(global-set-key (kbd "<C-M-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-S-right>")  'buf-move-right)



;;No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key (kbd "C-M-S-q") 'kill-this-buffer)
(global-set-key (kbd "C-M-S-x") 'multi-term)
(global-set-key (kbd "C-M-S-w") (lambda () (interactive)
                                  (kill-this-buffer)
                                  (if (equal 1 (length (window-list)))
                                      (delete-frame)
                                    (delete-window))))
(global-set-key (kbd "C-M-S-y") 'yas-describe-tables)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-M-<up>") 'text-scale-increase)
(global-set-key (kbd "C-M-<down>") 'text-scale-decrease)
(global-set-key (kbd "C-M-z C-e") 'eval-region)
(global-set-key (kbd "C-M-z C-M-e") 'eval-buffer)
(global-set-key (kbd "C-M-S-c") 'find-emacs-config)
(global-set-key (kbd "C-M-S-z") 'cmake-ide-compile)
(global-set-key [f5] 'recompile)
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
;;focus follow mouse
(setq mouse-autoselect-window 0
      focus-follows-mouse t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; See https://github.com/ch11ng/exwm/wiki
;; https://github.com/ch11ng/exwm/wiki/Configuration-Example
(when (boundp 'use-exwm)
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)
  ;(ido-mode 1)
  (require 'exwm)
  (require 'exwm-config)
  ;(exwm-config-ido)
  ;(exwm-config-default)
  (require 'exwm-randr)
  ;(add-hook 'exwm-randr-screen-change-hook
  ;        (lambda ()
  ;          (start-process-shell-command
  ;           "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  (require 'exwm-systemtray)
  ;(setq exwm-workspace-number 4)
  ;(setq mouse-autoselect-window nil)
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

(global-set-key [mode-line mouse-2] 'exwm-layout-toggle-fullscreen)



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
  (menu-bar-mode -1)
  (fringe-mode 1)
  (display-time-mode t))

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


(require 'company)
(cmake-ide-setup)
(setq company-backends (delete 'company-semantic company-backends))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'lisp-mode-hook 'company-mode)

(load "ext/google-styleguide/google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; autocompletion and highlighting modules
;(require 'python-mode)
(require 'cc-mode)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony company-lsp company-jedi)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

; TODO:
;   M-x rtags-install
;   M-x irony-install-server

(require 'company)
(setq company-async-timeout 5)
(require 'flycheck)
;(require 'rtags)
;(require 'flycheck-rtags)
(require 'flycheck-irony)
(require 'company-irony-c-headers)
;(setq rtags-completions-enabled t)
;(setq rtags-autostart-diagnostics t)
;(rtags-enable-standard-keybindings)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'company-mode)
(add-hook 'python-mode-hook 'company-mode)

;(defun my-flycheck-setup ()
;  (flycheck-select-checker 'rtags)
;  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;  (setq-local flycheck-check-syntax-automatically nil))
;(add-hook 'c-mode-common-hook #'my-flycheck-setup)

; company+irony autocompletion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0.5)
(define-key c-mode-map [(tab)] 'company-indent-or-complete-common)
(define-key c++-mode-map [(tab)] 'company-indent-or-complete-common)
;(define-key python-mode-map [(tab)] 'company-indent-or-complete-common)

;(defun my-irony-mode-hook ()
;  (define-key irony-mode-map [remap completion-at-point]
;    'irony-completion-at-point-async)
;  (define-key irony-mode-map [remap complete-symbol]
;    'irony-completion-at-point-async))

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

(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
(setq-default dired-mode-hook 'dired-hide-details-mode)
(setq-default show-paren-mode t)
