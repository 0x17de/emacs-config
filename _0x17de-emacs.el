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
(load "init-exwm.el")

(load "utils/exec-path-from-shell")
(load "utils/minibuffer")
(load "utils/indention")
(load "utils/replace-with-shell")
(load "utils/misc")
(load "utils/bolt")
(load "utils/vterm")
(load "utils/modeline")
(load "utils/multiple-cursors")

(defcustom _0x17de/M-x-library 'amx
  "The default M-x interface."
  :type '(choice (const simple)
                 (const smex)
                 (const amx))
  :group '_0x17de)
(pcase _0x17de/M-x-library
   ('smex
    (use-package smex
      :ensure t
      :bind
      (("M-x" . 'smex)
       ("M-X" . 'smex-major-mode-commands)
       ("C-M-x" . 'execute-extended-command))
      :init
      (smex-initialize)))
   ('amx
    (use-package amx
      :ensure t
      :init
      (amx-mode t)
      :custom
      (completion-styles '(flex basic partial-completion emacs22))
      (amx-completing-read-function #'completing-read)
      (amx-save-file (concat user-emacs-directory "amx-items"))
      (amx-history-length 50)))
   ('simple nil))

(load "langs/common")

(load "langs/ansible")
(load "langs/plantuml")
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

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
(load "ext/misc/vline")
(load "ext/misc/col-highlight")
(load "ext/misc/crosshairs")
