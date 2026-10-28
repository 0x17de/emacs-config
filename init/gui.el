;; -*- lexical-binding: t; -*-
;;No splash screen
(setq inhibit-startup-message t)

;; The init file picks a doom theme (load-theme after loading this config),
;; but nothing else pulls in the package providing it.
(use-package doom-themes
  :demand t)

;; Emacs 31 makes gnus-group-news-low inherit gnus-group-news-low-empty, while
;; doom-themes makes gnus-group-news-low-empty inherit gnus-group-news-low.
;; The resulting inheritance cycle errors on every frame creation and wedges
;; the daemon. Point the -empty face at gnus-group-mail-1-empty instead, like
;; doom-themes does for gnus-group-mail-low-empty.
(defun _0x17de/doom-break-gnus-face-cycle (theme)
  "Break the gnus-group-news-low face inheritance cycle in doom THEME."
  (when (string-prefix-p "doom-" (symbol-name theme))
    (custom-theme-set-faces
     theme
     '(gnus-group-news-low-empty ((t (:inherit gnus-group-mail-1-empty :weight normal)))))))
(mapc #'_0x17de/doom-break-gnus-face-cycle custom-enabled-themes)
(add-hook 'enable-theme-functions #'_0x17de/doom-break-gnus-face-cycle)

;;Disable bars
(defun simplify-ui (frame)
  "Remove unwanted menubar/scrollbar/etc from FRAME."
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
