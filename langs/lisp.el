(require 'lisp-mode)
(add-hook 'lisp-mode-hook
          (lambda ()
            (make-local-variable 'company-backends)
            (setq company-backends '(company-capf
                                     company-files))
            (company-mode t)))
(define-key lisp-mode-map [(tab)] 'company-indent-or-complete-common)

(require 'elisp-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'company-backends)
            (setq company-backends '(company-elisp
                                     company-capf
                                     company-files))
            (company-mode t)))
(define-key emacs-lisp-mode-map [(tab)] 'company-indent-or-complete-common)
