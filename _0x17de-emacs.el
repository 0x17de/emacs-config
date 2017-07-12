;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;
; (add-to-list 'load-path "~/.emacs.d/_0x17de/")
; (load "_0x17de-emacs")

(load "melpa-source")

;;initial setup of recommended packages i use
(defvar dotemacs-packages
  '(doremi doremi-cmd doremi-frm multiple-cursors cmake-mode color-theme-sanityinc-tomorrow)
  "All packages i require")
(defvar dotemacs-needs-install nil)
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

