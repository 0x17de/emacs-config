(defvar _0x17de/vterm-counter 0
  "Counter to make unique vterm buffers.")
(defun _0x17de/start-vterm ()
    "Starts a new vterm buffer."
  (interactive)
  (setq _0x17de/vterm-counter (1+ _0x17de/vterm-counter))
  (let ((buffer-name (format "*vterm-%d*" _0x17de/vterm-counter)))
    (vterm buffer-name)))
(use-package vterm
  :ensure t
  :bind (("C-M-S-x" . _0x17de/start-vterm)
         :map vterm-mode-map
         ("C-c C-x" . 'vterm--self-insert)))
