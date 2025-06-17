;; -*- lexical-binding: t; -*-
(defun my/latex-setup ()
  "Setup the latex environment"
  (reftex-mode t)
  (auctex-latexmk-setup)
  (TeX-engine-set "luatex")
  (setq font-latex-fontify-script 'multi-level)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (TeX-fold-mode 1)
  (rainbow-delimiters-mode t)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-auctex
                           company-files))
  (company-auctex-init)
  (company-mode t))
(defun my/latexmk-run ()
  "Run latexmk on the master file"
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error)
      (minibuffer-message "latexmk-done"))))
(use-package auctex-latexmk
  :defer t)
(use-package company-auctex
  :defer t)

(use-package latex
  :ensure nil
  :defer t
  :hook (TeX-mode . my/latex-setup)
  :bind (:map TeX-mode-map
              ([f5] . my/latexmk-run)
              ([tab] . company-indent-or-complete-common)
              ([f1] . company-show-doc-buffer))
  :config
  (load "latex-compile-on-save.el"))
