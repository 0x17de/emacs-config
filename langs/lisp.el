(use-package lisp-mode
  :ensure nil
  :bind
  (:map lisp-mode-map
        ([tab] . company-indent-or-complete-common))
  :hook
  (lisp-mode . (lambda ()
                 (setq company-backends '(company-capf
                                          company-files))
                 (company-mode t))))

(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ([tab] . company-indent-or-complete-common))
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq company-backends '(company-elisp
                                                company-capf
                                                company-files))
                       (company-mode t))))
