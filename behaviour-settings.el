;;No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;Yes or no short
(defalias 'yes-or-no-p 'y-or-n-p)

;;Just kill buffer without asking
(global-set-key [(control x) (k)] 'kill-this-buffer)
(global-set-key (kbd "C-M-<up>") 'text-scale-increase)
(global-set-key (kbd "C-M-<down>") 'text-scale-decrease)

;;Git diff fix
(setq vc-handled-backends ())

;;Autocomplete changes
(setq icomplete-mode t)
;(setq completion-cycle-threshold t)"
(setq completion-auto-help t)

;;Selection/Clipboard improvements
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
