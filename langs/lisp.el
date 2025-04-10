(defcustom _0x17de/lisp-paredit:enabled nil
  "When enabled, this option automatically activates paredit mode in Lisp modes."
  :group '_0x17de
  :type 'boolean)

(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf))
  :bind (:map lisp-mode-map
              ([tab] . slime-complete-symbol)
              ("C-c C-d h" . slime-documentation-lookup)
              ("C-c C-z" . slime-switch-to-output-buffer))
  :hook ((lisp-mode . (lambda ()
                        ;;(setq lisp-indent-function nil)
                        (slime-mode t)))
         (slime-repl-mode . (lambda ()
                              (company-mode t)))))

(use-package lisp-mode
  :ensure nil
  :demand t
  :bind
  (:map lisp-mode-map
        ([tab] . company-indent-or-complete-common))
  :hook
  (lisp-mode . (lambda ()
                 (setq company-backends '(company-slime
                                          company-capf
                                          company-files))
                 (highlight-indent-guides-mode t)
                 (company-mode t))))

(use-package elisp-mode
  :ensure nil
  :demand t
  :bind (:map emacs-lisp-mode-map ("<tab>" . 'company-indent-or-complete-common))
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

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy)
  (setq slime-company-major-modes '(lisp-mode slime-repl-mode)))

(when _0x17de/lisp-paredit:enabled
  (use-package paredit
    :ensure t
    :defer t
    ;;:diminish paredit-mode
    :hook ((emacs-lisp-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode)
           (slime-repl-mode . paredit-mode))
    :bind (:map paredit-mode-map
                ("C-M-f" . paredit-forward)
                ("C-M-b" . paredit-backward))))
