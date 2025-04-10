(defcustom _0x17de/M-x-library 'amx
  "The default M-x interface."
  :type '(choice (const simple)
                 (const smex)
                 (const amx))
  :group '_0x17de)
(pcase _0x17de/M-x-library
   ('smex
    (use-package smex
      :ensure t
      :bind
      (("M-x" . 'smex)
       ("M-X" . 'smex-major-mode-commands)
       ("C-M-x" . 'execute-extended-command))
      :init
      (smex-initialize)))
   ('amx
    (use-package amx
      :ensure t
      :init
      (amx-mode t)
      :custom
      (completion-styles '(flex basic partial-completion emacs22))
      (amx-completing-read-function #'completing-read)
      (amx-save-file (concat user-emacs-directory "amx-items"))
      (amx-history-length 50)))
   ('simple nil))
