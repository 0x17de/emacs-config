;; -*- lexical-binding: t; -*-
;;No splash screen
(setq inhibit-startup-message t)

;;Disable bars
(defun simplify-ui (frame)
  (interactive)
  "Remove unwanted menubar/scrollbar/etc"
  (modify-frame-parameters frame
                           '((menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'simplify-ui)
(when (display-graphic-p)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'fringe-mode) (fringe-mode 1))
  (if (fboundp 'display-time-mode) (display-time-mode t)))
