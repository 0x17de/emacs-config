;; see https://github.com/company-mode/company-mode/issues/525#issuecomment-348635719
(defun inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    (message "%s" result)
    result))
(defun inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (let ((result (nth 4 (syntax-ppss))))
    (message "%s" result)
    result))
(defun semantic-completion-advice (adviced-f &rest r)
  "Check if POINT it's inside a string or comment before calling semantic-*"
  (unless (or (inside-string-q) (inside-comment-q))
    (apply adviced-f r)))

(use-package dumb-jump
  :init
  (global-set-key (kbd "C-S-j") 'dumb-jump-go))
(use-package semantic
  :config
  (advice-add 'semantic-analyze-completion-at-point-function
              :around #'semantic-completion-advice)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-idle-completions-mode -1)
  (semantic-mode 1))

(use-package srefactor)
;(use-package auto-complete
;  :init
;  (setq tab-always-indent 'complete)
;  (setq ac-disable-inline t)
;  :config
;  (setq ac-sources (delete 'ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(use-package auctex
;  :ensure t)
;; (use-package auto-complete-auctex)
(use-package company-quickhelp)
(defun company-init-quickhelp (b-enable)
  "Initialize company-quickhelp only if company is running"
  (company-quickhelp-mode t)
  (remove-hook 'company-completion-started-hook 'company-init-quickhelp t))
(use-package company
  :init 
  (setq company-async-timeout 5
        company-dabbrev-downcase 0
        company-idle-delay 0.5
        company-irony-ignore-case t
        company-require-match nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  :config
  (add-hook 'company-completion-started-hook 'company-init-quickhelp)
  (company-tng-mode t)
  (setq company-backends '())) ; we explicitly add all the backends we need

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config (setq lsp-prefer-flymake nil
                lsp-file-watch-threshold 4000))
(use-package flycheck)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 150)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-border "white")

  ;; Use mouse events to trigger and navigate doc
  (define-key lsp-ui-doc-frame-mode-map [mouse-1] #'ignore)
  (define-key lsp-ui-doc-frame-mode-map [mouse-3] #'lsp-ui-doc-hide)
  (define-key lsp-ui-doc-frame-mode-map [mouse-wheel-up] #'lsp-ui-doc-scroll-up)
  (define-key lsp-ui-doc-frame-mode-map [mouse-wheel-down] #'lsp-ui-doc-scroll-down))

;; Autocomplete changes
(setq icomplete-mode t)
;; (setq completion-cycle-threshold t)"
(setq completion-auto-help t)
