;; -*- lexical-binding: t; -*-
;; Startup GC and file name handler tweaks live in early-init.el.

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (* 16 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
