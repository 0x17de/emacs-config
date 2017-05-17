;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;
; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
; (load "_0x17de-emacs")

(load "melpa-source")

;;initial setup of recommended packages i use
(defun dotemacs-install()
  (interactive)
  (package-install #'doremi)
  (package-install #'doremi-cmd)
  (package-install #'doremi-frm)
  (package-install #'multiple-cursors)
  (package-install #'cmake-mode)
  (package-install #'color-theme-sanityinc-tomorrow))

(load "ext/tex-switch-quotes/tex-switch-quotes")

(load "behaviour-settings")
(load "indentation-settings")
(load "display-settings")
(load "mouse-settings")

;(load "ext/google-styleguide/google-c-style")
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;(when
;(require 'auto-complete-c-headers)
;(add-to-list 'ac-sources 'ac-source-c-headers)

(load "compile-helper-functions")
;;Compile hotkeys
(global-set-key (kbd "C-M-z C-a") 'compile-cmake)
(global-set-key (kbd "C-M-z C-M-a") 'compile-make)
(global-set-key (kbd "C-M-z C-s") 'compile-policy)

(load "cpp-helper-functions")

(load "directory-helper-functions")

(load "clipboard-helper-functions")
(add-hook 'minibuffer-setup-hook 'my/paste-in-minibuffer)

(load "emacs-settings")

