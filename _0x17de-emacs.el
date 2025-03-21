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
(load "init-zoom-frm.el")

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(load "utils/indention")
(load "utils/replace-with-shell")
(load "utils/themes")
(load "utils/misc")

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))
(use-package vterm
  :ensure t
  :bind (("C-M-S-x" . 'vterm)
         :map vterm-mode-map
         ("C-c C-x" . 'vterm--self-insert)))
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t))

(load "utils/multiple-cursors")
;;(use-package sudo-edit)
(use-package refine)
(use-package function-args)
(use-package helm-swoop)
(use-package helm-gtags)
(use-package counsel)

(use-package stickyfunc-enhance)
(use-package smex
  :ensure t
  :bind
  (("M-x" . 'smex)
   ("M-X" . 'smex-major-mode-commands)
   ("C-M-x" . 'execute-extended-command))
  :init
  (smex-initialize))

(load "langs/common")

(load "langs/ansible")
(load "langs/lisp")
(load "langs/java")
(load "langs/go")
(load "langs/c")
(load "langs/python")
(load "langs/rust")
(load "langs/org")
(load "langs/xml")
(load "langs/misc")
(load "ext/ox-confluence/ox-confluence")

(load "langs/latex")

;;(global-set-key (kbd "C-c w") 'whitespace-mode)
;;(windmove-default-keybindings)

;; notes: speedbar, sr-speedbar

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
(load "ext/misc/vline")
(load "ext/misc/col-highlight")
(load "ext/misc/crosshairs")
