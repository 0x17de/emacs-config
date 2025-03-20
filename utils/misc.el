(ido-mode t)

(global-unset-key (kbd "C-z")) ; stop me from freezing emacs
;; Always group items in buffer menu
(setq mouse-buffer-menu-mode-mult 0)
;; Use ibuffer instead of temporary buffer popup
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
;(global-set-key (kbd "<home>") 'smart-beginning-of-line)

;; avoid help-for-help via f1
(global-unset-key [(f1)])

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key (kbd "C-M-S-q") (lambda () (interactive) (kill-this-buffer)))
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
(global-set-key (kbd "C-M-S-u") 'rename-uniquely)
(global-set-key (kbd "C-M-z C-e") 'eval-region)
(global-set-key (kbd "C-M-z C-M-e") 'eval-buffer)
(global-set-key (kbd "C-M-S-c") 'find-emacs-config)
(global-set-key (kbd "C-M-S-z") 'cmake-ide-compile)
(defun find-emacs-config ()
  "Open the emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/_0x17de/_0x17de-emacs.el"))

;; Selection/Clipboard improvements
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)
;;focus follow mouse
(setq mouse-autoselect-window 0
      focus-follows-mouse t)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

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

;; Git diff fix
(setq vc-handled-backends ())

(load "init-exwm.el")
;;Term or GUI mode mouse changes
(unless (display-graphic-p)
  (require 'mouse)
  ;;http://stackoverflow.com/questions/3466643/emacs-unicode-xterm-mouse-escape-sequences-and-wide-terminals
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(load "directory-helper-functions")

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;(customize-set-variable 'dired-mode-hook (quote (dired-hide-details-mode)))
;(customize-set-variable 'show-paren-mode t)
;(customize-set-variable 'custom-enabled-themes '(gruvbox))
;(customize-set-variable 'ansi-color-faces-vector [default default default italic underline success warning error])
;(customize-set-variable 'ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
(put 'downcase-region 'disabled nil)

(use-package buffer-move
  :config
  (global-set-key (kbd "<C-M-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-M-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-M-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-M-S-right>")  'buf-move-right))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))
