;; -*- lexical-binding: t; -*-
;; Tab fix
(global-set-key (kbd "<backtab>") 'insert-tab-char)
(global-set-key (kbd "<C-tab>") 'company-complete)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
;; Tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq default-tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Auto indentation
(add-to-list 'load-path "guess-style")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
