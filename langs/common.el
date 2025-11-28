;; -*- lexical-binding: t; -*-
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

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'fill)
  (set-face-background 'highlight-indent-guides-odd-face "gray14")
  (set-face-background 'highlight-indent-guides-even-face "gray18")
  (set-face-foreground 'highlight-indent-guides-character-face "gray16"))

(use-package dumb-jump
  :init
  (global-set-key (kbd "C-S-j") 'dumb-jump-go))
(use-package semantic
  :ensure nil
  :config
  (advice-add 'semantic-analyze-completion-at-point-function
              :around #'semantic-completion-advice)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-idle-completions-mode -1)
  (semantic-mode 1))

(use-package srefactor
  :defer t)
;(use-package auto-complete
;  :init
;  (setq tab-always-indent 'complete)
;  (setq ac-disable-inline t)
;  :config
;  (setq ac-sources (delete 'ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(use-package auctex
;  :ensure t)
;; (use-package auto-complete-auctex)
(use-package company-quickhelp
  :defer t)
(defun company-init-quickhelp (b-enable)
  "Initialize company-quickhelp only if company is running"
  (company-quickhelp-mode t)
  (remove-hook 'company-completion-started-hook 'company-init-quickhelp t))
(use-package company
  :defer t
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
  (setq company-backends '())
  (add-to-list 'company-backends 'company-capf)) ; LSP completion backend

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :config (setq lsp-prefer-flymake nil
                lsp-enable-file-watchers nil
                lsp-enable-multi-root nil
                lsp-enable-symbol-highlighting nil
                lsp-headerline-breadcrumb-enable nil
                lsp-file-watch-threshold 4000))
(use-package flycheck
  :defer t)

(defgroup _0x17de/mmm nil
  "mmm mode configuration."
  :group '_0x17de
  :prefix "_0x17de/mmm-")
(defcustom _0x17de/mmm-mode:enable nil
  "Enable mmm-mode."
  :group '_0x17de/mmm
  :type 'boolean)
(defcustom _0x17de/mmm-mode:babel:enable nil
  "Enable org-babel support for mmm-mode."
  :group '_0x17de/mmm
  :type 'boolean)

(when _0x17de/mmm-mode:enable
  (defun mmm-get-mode-for-lang (lang)
    (cond ((string= lang "python") 'python-mode)
          ((string= lang "elisp") 'emacs-lisp-mode)
          ((string= lang "emacs-lisp") 'emacs-lisp-mode)
          ((string= lang "js") 'js-mode)
          ((string= lang "ruby") 'ruby-mode)
          ;; Add more language mappings as needed
          (t nil)))
  (use-package mmm-mode
    :config
    (setq mmm-global-mode 'maybe)
    (mmm-add-classes
     '((org-babel
        :submode nil
        :front "#\\+\\(begin_src\\|BEGIN_SRC\\) \\([a-zA-Z+-]+\\)"
        :front-match 2
        :front-offset 0
        :back "#\\+\\(end_src\\|END_SRC\\)"
        :save-matches t
        :delimiter-mode nil
        :match-submode (lambda (lang)
                         (let ((l (match-string-no-properties 2)))
                           (mmm-get-mode-for-lang l))))))
    (when _0x17de/mmm-mode:babel:enable
      (mmm-add-mode-ext-class 'org-mode nil 'org-babel)
      (mmm-add-mode-ext-class 'org-journal-mode nil 'org-babel))))

(use-package lsp-ui
  :defer t
  :bind (:map lsp-ui-doc-frame-mode-map          
    ([mouse-1] . #'ignore)
    ([mouse-3] . #'lsp-ui-doc-hide)
    ([mouse-wheel-up] . #'lsp-ui-doc-scroll-up)
    ([mouse-wheel-down] . #'lsp-ui-doc-scroll-down))
  :config
  (with-eval-after-load 'lsp-mode
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse t)
    (setq lsp-ui-doc-position 'at-point)
    (setq lsp-ui-doc-delay 0.5)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-use-webkit nil)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-max-width 150)
    (setq lsp-ui-doc-max-height 30)
    (setq lsp-ui-doc-header t)
    (setq lsp-ui-doc-border "white")))

;; Autocomplete changes
(setq icomplete-mode t)
;; (setq completion-cycle-threshold t)"
(setq completion-auto-help t)
