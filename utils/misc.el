;; -*- lexical-binding: t; -*-
(use-package savehist
  :ensure nil
  :init
  (savehist-mode t)
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory)))

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

;; handle backup files
(defcustom _0x17de/backup-file-location "~/.emacs.d/backups"
  "Directory where backup files will be stored.
This specifies the path where Emacs will save backup files when
editing. The default location is in the .emacs.d directory."
  :type 'directory
  :group '_0x17de)
(condition-case nil
    (make-directory _0x17de/backup-file-location t)
  (error (message "Warning: Could not create backup directory")))
(setq backup-directory-alist (list (cons "." _0x17de/backup-file-location)))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key (kbd "C-M-<escape>") (lambda () (interactive) (kill-emacs)))
(global-set-key (kbd "C-M-S-q") (lambda () (interactive) (kill-this-buffer)))
(global-set-key (kbd "C-M-S-w") (lambda () (interactive)
                                  (kill-this-buffer)
                                  (if (equal 1 (length (window-list)))
                                      (delete-frame)
                                    (delete-window))))
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-this-buffer)))
(global-set-key (kbd "C-M-<up>") (lambda () (interactive)
                                   (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 5))))
(global-set-key (kbd "C-M-<down>") (lambda () (interactive)
                                     (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 5))))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
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
(defcustom _0x17de/focus-follows-mouse t
  "When non-nil, focus follows the mouse pointer."
  :type 'boolean
  :group '_0x17de)
(setq mouse-autoselect-window 0
      focus-follows-mouse _0x17de/focus-follows-mouse)
;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;Diff adjustements
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")
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
(defcustom _0x17de/scroll-amount 5
  "Scolling up/down will jump by this amount."
  :type 'integer
  :group '_0x17de)
(defun up-slightly () (interactive) (scroll-up _0x17de/scroll-amount))
(defun down-slightly () (interactive) (scroll-down _0x17de/scroll-amount))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;; Git diff fix
(setq vc-handled-backends ())

;;Term or GUI mode mouse changes
(unless (display-graphic-p)
  (require 'mouse)
  ;;http://stackoverflow.com/questions/3466643/emacs-unicode-xterm-mouse-escape-sequences-and-wide-terminals
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package which-key
  :demand t
  :init
  (setq which-key-idle-delay 0.3)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-max-description-length 40)
  :config
  (which-key-mode 1))

(use-package ace-window
  :demand t
  :bind
  (("C-x o" . ace-window)))

(defvar _0x17de/window-toggle-fullscreen:state nil)
(defun _0x17de/window-toggle-fullscreen ()
  (interactive)
  (if _0x17de/window-toggle-fullscreen:state
      (progn
        (winner-undo)
        (setq _0x17de/window-toggle-fullscreen:state nil))
    (progn
      (delete-other-windows)
      (setq _0x17de/window-toggle-fullscreen:state t))))
(when (boundp 'winner-mode)
  (winner-mode t))
(global-set-key (kbd "S-<f11>") '_0x17de/window-toggle-fullscreen)

(use-package buffer-move
  :bind
  (("C-M-S-<up>"    . buf-move-up)
   ("C-M-S-<down>"  . buf-move-down)
   ("C-M-S-<left>"  . buf-move-left)
   ("C-M-S-<right>" . buf-move-right)))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 0.2))

(use-package all-the-icons
  :ensure t
  :demand t)

(use-package refine)
(use-package function-args)
(use-package stickyfunc-enhance)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))
(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package avy
  :ensure t
  :demand t
  :bind (("C-:" . avy-goto-char)
         ("C-M-:" . avy-goto-char-timer)))

(use-package persp-mode
  :ensure t
  :demand t
  :custom
  (wg-morph-on nil)
  (persp-autokill-buffer-on-remove 'kill-weak)
  (persp-auto-resume-time -1.0)
  (persp-emacsclient-init-frame-behaviour-override nil)
  (persp-nil-name "main")
  :init
  (persp-mode t)
  :config
  (persp-set-keymap-prefix (kbd "C-c P")))

(global-set-key
 (kbd "C-c b")
 (lambda ()
   (interactive)
   (let ((url (thing-at-point 'url))
         (filename (thing-at-point 'filename)))
     (cond
      (url (browse-url url))
      (filename (find-file filename))
      (t (message "No URL or file found"))))))

(use-package origami
  :ensure t
  :defer t
  :config
  (global-origami-mode t))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
