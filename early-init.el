;; -*- lexical-binding: t; -*-
;; Loaded from ~/.emacs.d/early-init.el, before package.el activates packages
;; and before the first frame exists. Nothing here may depend on a package.

;; Defer GC and skip file name handlers (jka-compr, tramp) for the whole
;; startup. Handlers registered while loading are kept on restore.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(defvar _0x17de/file-name-handler-alist file-name-handler-alist
  "`file-name-handler-alist' as it was before startup.")
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       _0x17de/file-name-handler-alist)))))

;; Set the bars off in the frame parameters, so the first frame is never drawn
;; with them. init/gui.el still turns the modes off for setups without this
;; file.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
;; Do not resize the frame to keep its columns/lines when fonts load.
(setq frame-inhibit-implied-resize t)
