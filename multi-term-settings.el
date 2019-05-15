(setq multi-term-scroll-to-bottom-on-output nil)
(setq multi-term-program "/bin/zsh")
(setq multi-term-switch-after-close nil) ;do not switch to next terminal on when killing term

(set-terminal-coding-system 'utf-8-unix)
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(defun term-handle-exit--close-buffer (&rest args)
  (when (null (get-buffer-process (current-buffer)))
    (insert "Press <C-d> to kill the buffer.")
    (use-local-map (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "C-d")
                       (lambda ()
                         (interactive)
                         (kill-buffer (current-buffer))))
                     map))))

(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer)

(add-hook 'term-mode-hook
          (lambda ()
            (set-window-dedicated-p (selected-window) 1)
            (setq term-eol-on-send nil) ;allows me to hit return in middle of command and not have duplicate content sent to shell
            (define-key term-raw-map (kbd "C-y")   'term-paste) ;to make yank work properly
            (define-key term-raw-map (kbd "M-SPC") 'ric/term-toggle-line-char-mode)
            (define-key term-raw-map (kbd "M-DEL") 'term-send-raw-meta) ;actually kill in shell instead of just buffer
            (define-key term-raw-map (kbd "M-d")   'term-send-raw-meta)
            (define-key term-raw-map (kbd "M-<left>")   'term-send-backward-word)
            (define-key term-raw-map (kbd "M-<right>")   'term-send-forward-word)
            (define-key term-raw-map (kbd "M-<backspace>")   'term-send-backward-kill-word)
            (define-key term-raw-map (kbd "M-b")   'term-send-backward-word)
            (define-key term-raw-map (kbd "M-f")   'term-send-forward-word)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-z p" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("C-z n" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-z e" . term-send-esc))
            (add-to-list 'term-bind-key-alist '("C-z z" . term-send-ctrl-z))))

(defun term-send-jump-word-left ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e\e[D"))

(defun term-send-jump-word-right ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e\e[C"))

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun term-send-ctrl-z ()
  "Allow using ctrl-z to suspend in multi-term shells."
  (interactive)
  (term-send-raw-string ""))

(defun term-toggle-line-char-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun kill-ring-save-switch-to-char-mode (b e)
  "In line-mode, M-w also switches back to char-mode and goes back to prompt."
  (interactive "r")
  (kill-ring-save b e t)
  (when (term-in-line-mode)
    (term-char-mode)
    (term-send-raw-string "")))
