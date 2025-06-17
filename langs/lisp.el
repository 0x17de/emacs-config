;; -*- lexical-binding: t; -*-
(defcustom _0x17de/lisp-paredit:enabled nil
  "When enabled, this option automatically activates paredit mode in Lisp modes."
  :group '_0x17de
  :type 'boolean)

(use-package sly
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (setq sly-complete-symbol-function 'sly-flex-completions
  :hook ((lisp-mode . (lambda ()
                        (sly-mode t)))
         (sly-repl-mode . (lambda ()
                            (company-mode t))))))

(use-package lisp-mode
  :ensure nil
  :demand t
  :bind
  (:map lisp-mode-map
        ;;([tab] . sly-complete-symbol)
        ([tab] . company-indent-or-complete-common)
        ("C-c C-d h" . sly-documentation-lookup)
        ("C-c C-z" . sly))
  :hook
  ((lisp-mode . (lambda ()
                 (setq company-backends '(company-capf
                                          company-files))
                 (highlight-indent-guides-mode t)
                 (company-mode t)))))

(use-package elisp-mode
  :ensure nil
  :demand t
  :bind (:map emacs-lisp-mode-map ([tab] . 'company-indent-or-complete-common))
  :hook
  (emacs-lisp-mode . (lambda ()
                       (company-mode t)
                       (eldoc-mode t)
                       (prettify-symbols-mode t)
                       ;;(setq lisp-indent-function nil)
                       (highlight-indent-guides-mode t)
                       (setq company-backends '(company-capf
                                                company-files
                                                company-dabbrev-code)))))

(when _0x17de/lisp-paredit:enabled
  (use-package paredit
    :ensure t
    :defer t
    ;;:diminish paredit-mode
    :hook ((emacs-lisp-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode)
           (sly-repl-mode . paredit-mode))
    :bind (:map paredit-mode-map
                ("C-M-f" . paredit-forward)
                ("C-M-b" . paredit-backward))))
