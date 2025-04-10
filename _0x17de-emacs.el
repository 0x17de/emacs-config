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
(load "utils/extended-command")

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

(load "langs/latex")

;;(global-set-key (kbd "C-c w") 'whitespace-mode)
;;(windmove-default-keybindings)

(load "ext/tex-switch-quotes/tex-switch-quotes")
(load "ext/misc/hl-line+")
