;;Tab fix
;(global-set-key (kbd "TAB") 'insert-tab-char) ; same as Ctrl+i
;(global-set-key (kbd "<tab>") 'insert-tab-char)
;(electric-indent-mode 0)
(global-set-key (kbd "<backtab>") 'insert-tab-char)
;;Tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;Auto indentation
(add-to-list 'load-path "guess-style")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
;(setq-default tab-always-indent nil)

;;Helper functions
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
