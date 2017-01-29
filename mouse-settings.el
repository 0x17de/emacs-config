;;Multi cursor edit
(when (require 'multiple-cursors nil 'noerror)
      (global-set-key (kbd "C-M-z C-c") 'mc/edit-lines)
      (global-set-key (kbd "C-M-z >") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-M-z <") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-M-z |") 'mc/mark-all-like-this)
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

;;Mouse scroll
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;;Term or GUI mode mouse changes
(unless (display-graphic-p)
        (require 'mouse)
;;http://stackoverflow.com/questions/3466643/emacs-unicode-xterm-mouse-escape-sequences-and-wide-terminals
        (xterm-mouse-mode t)
        (defun track-mouse (e))
        (setq mouse-sel-mode t))
;(mouse-choose-completion)
