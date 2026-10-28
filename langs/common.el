;; -*- lexical-binding: t; -*-
(defvar _0x17de/treesit-modes
  '((python-ts-mode . python)
    (go-ts-mode . go)
    (rust-ts-mode . rust))
  "Tree-sitter major modes to prefer, each with the grammar it needs.
C and C++ stay on cc-mode: c-ts-mode cannot express google-c-style and
the overrides in `_0x17de/google-c-style-overrides'.")
;; Only where the grammar is installed (the Nix config links them into
;; ~/.emacs.d/tree-sitter); elsewhere the classic modes stay.
(when (and (treesit-available-p) (boundp 'treesit-enabled-modes))
  (setopt treesit-enabled-modes
          (mapcar #'car (seq-filter (lambda (m) (treesit-language-available-p (cdr m)))
                                    _0x17de/treesit-modes))))
(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'fill)
  (set-face-background 'highlight-indent-guides-odd-face "gray14")
  (set-face-background 'highlight-indent-guides-even-face "gray18")
  (set-face-foreground 'highlight-indent-guides-character-face "gray16"))

(use-package dumb-jump
  :defer t
  :init
  ;; Global, so it only answers after mode-local backends such as lsp-mode's.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (global-set-key (kbd "C-S-j") #'xref-find-definitions))
;(use-package auto-complete
;  :init
;  (setq tab-always-indent 'complete)
;  (setq ac-disable-inline t)
;  :config
;  (setq ac-sources (delete 'ac-sources '(ac-source-semantic ac-source-semantic-raw))))
;(use-package auctex
;  :ensure t)
;; (use-package auto-complete-auctex)
;; In-buffer completion. Every mode feeds `completion-at-point-functions', so
;; there is no per-mode backend list; TAB indents, then completes.
(setq tab-always-indent 'complete)
(defun _0x17de/corfu-tab ()
  "Expand the snippet key before point, else select the next candidate.
The popup keymap shadows `yas-minor-mode-map', so without this TAB only
expanded snippets when it beat the popup. Only while no candidate is
selected: the key is still in the buffer then, and TAB cycles after."
  (interactive)
  (if (and (< corfu--index 0)
           (bound-and-true-p yas-minor-mode)
           (yas-maybe-expand-abbrev-key-filter t))
      (progn
        (corfu-quit)
        (yas-expand))
    (corfu-next)))
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  ;; Tab-and-go: nothing is selected until TAB, and the selected candidate is
  ;; inserted as a preview that typing on simply keeps.
  (corfu-preselect 'prompt)
  ;; Where company was enabled before; text modes like org stay quiet.
  (global-corfu-modes '(prog-mode TeX-mode))
  :bind
  (:map corfu-map
        ("TAB" . _0x17de/corfu-tab)
        ([tab] . _0x17de/corfu-tab)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ([f1] . corfu-info-documentation))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))
(use-package cape
  :demand t
  :init
  ;; Global, so it is tried after each mode's own completion functions.
  (add-hook 'completion-at-point-functions #'cape-file))

(defun _0x17de/lsp-completion-orderless ()
  "Match LSP completions with orderless, like the minibuffer does."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
;; lsp-mode picks plists or hash tables for LSP messages when it is compiled
;; and reads this again when it loads, so it has to be set before either.
;; Recompile lsp-mode and its dependents after changing it.
(setenv "LSP_USE_PLISTS" "true")
;; emacs-lsp-booster sits between Emacs and the server, parses the JSON off
;; the main thread and hands Emacs ready-made bytecode.
;; https://github.com/blahgeek/emacs-lsp-booster
(defun _0x17de/lsp-booster-parse (old-fn &rest args)
  "Read bytecode from emacs-lsp-booster, else parse JSON with OLD-FN."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
(defun _0x17de/lsp-booster-command (old-fn cmd &optional test?)
  "Run the server command from OLD-FN for CMD under emacs-lsp-booster."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             ;; Remote commands get a shell wrapper the booster cannot see past.
             (not (file-remote-p default-directory))
             lsp-use-plists
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command (executable-find (car orig-result))))
            (setcar orig-result command))
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook (lsp-completion-mode . _0x17de/lsp-completion-orderless)
  ;; Same keys in every language with a server.
  :bind (:map lsp-mode-map
              ([f1] . lsp-describe-thing-at-point)
              ([f12] . lsp-find-definition)
              ("S-<f12>" . lsp-find-references))
  :config (setq lsp-prefer-flymake nil
                ;; Plain capf for corfu; :capf would set up company.
                lsp-completion-provider :none
                lsp-enable-file-watchers t
                lsp-enable-multi-root nil
                lsp-enable-symbol-highlighting nil
                lsp-headerline-breadcrumb-enable nil
                lsp-file-watch-threshold 10000)
          (setq lsp-client-packages (delete 'lsp-java lsp-client-packages))
          (advice-add 'json-parse-buffer :around #'_0x17de/lsp-booster-parse)
          (advice-add 'lsp-resolve-final-command :around
                      #'_0x17de/lsp-booster-command))
(use-package flycheck
  :defer t)

;; Format on save, asynchronously and without moving point. Enabled per
;; language by `apheleia-mode' in its hook, not globally: a global mode would
;; also run clang-format or rustfmt over projects that never asked for it.
(use-package apheleia
  :defer t
  :config
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) '(ruff-isort ruff)))
  (dolist (mode '(go-mode go-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'goimports)))

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
(icomplete-mode -1)
;; (setq completion-cycle-threshold t)"
(setq completion-auto-help t)
