;; -*- lexical-binding: t; -*-
;; Tab fix
(global-set-key (kbd "<backtab>") 'insert-tab-char)
(global-set-key (kbd "<C-tab>") #'completion-at-point)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
;; Tab width
(setq-default indent-tabs-mode nil)
;; setq-default, not setq: tab-width is automatically buffer-local, so a plain
;; setq here only ever applied to whichever buffer was current during startup.
(setq-default tab-width 4)
;; NB: no alias for c-basic-offset. Aliasing it to tab-width coupled cc-mode
;; indentation to the display tab width, so go-mode's (setq-local tab-width 2)
;; silently reindented C/C++. cc-mode sets it per-mode instead.
(defvaralias 'cperl-indent-level 'tab-width)
