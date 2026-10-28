;; -*- lexical-binding: t; -*-
(defcustom _0x17de/lisp-paredit:enabled nil
  "When enabled, this option automatically activates paredit mode in Lisp modes."
  :group '_0x17de
  :type 'boolean)

(use-package lisp-mode
  :ensure nil
  :demand t
  :hook
  (lisp-mode . (lambda ()
                 (highlight-indent-guides-mode t))))

(use-package elisp-mode
  :ensure nil
  :demand t
  :hook
  (emacs-lisp-mode . (lambda ()
                       (eldoc-mode t)
                       (prettify-symbols-mode t)
                       ;;(setq lisp-indent-function nil)
                       (highlight-indent-guides-mode t)
                       ;; After elisp's own completion, like company-dabbrev-code.
                       (add-hook 'completion-at-point-functions
                                 #'cape-dabbrev 90 t))))

(when _0x17de/lisp-paredit:enabled
  (use-package paredit
    :ensure t
    :defer t
    ;;:diminish paredit-mode
    :hook ((emacs-lisp-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode))
    :bind (:map paredit-mode-map
                ("C-M-f" . paredit-forward)
                ("C-M-b" . paredit-backward))))
