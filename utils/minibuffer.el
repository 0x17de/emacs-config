;; -*- lexical-binding: t; -*-
(defcustom _0x17de/minibuffer-completion-framework 'vertico
  "The completion framework to use in minibuffer."
  :type '(choice (const ido :tag "Ido (Classic)")
                 (const vertico :tag "Vertico + Consult"))
  :group '_0x17de)
(pcase _0x17de/minibuffer-completion-framework
  ('ido
   (require 'ido)
   (ido-mode t)
   (setq ido-everywhere t)
   (setq ido-enable-flex-matching t)
   (use-package ido-completing-read+
     :ensure t
     :demand t
     :config
     (ido-ubiquitous-mode 1))
   (use-package ido-vertical-mode
     :ensure t
     :demand t
     :init (ido-vertical-mode 1)))
  ('vertico
   (use-package vertico
     :ensure t
     :demand t
     :init
     (vertico-mode)
     :custom
     (vertico-count 25)
     (vertico-resize t))
   (use-package consult
     :ensure t
     :demand t
     :bind (("C-x b" . consult-buffer)
            ("C-x 4 b" . consult-buffer-other-window)
            ("C-x r b" . consult-bookmark)
            ("M-y" . consult-yank-pop)
            ("M-g g" . consult-goto-line)
            ("M-s r" . consult-ripgrep)))))
