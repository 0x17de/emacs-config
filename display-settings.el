;;No splash screen
(setq inhibit-startup-message t)

;;Disable bars
(when (display-graphic-p)
      (scroll-bar-mode -1)
      (tool-bar-mode -1))
(menu-bar-mode -1)

;;Diff adjustements
(setq ediff-split-window-function 'split-window-horizontally)
;;/etc/etc-update.conf
;diff_command="emacs-diff %file1 %file2"
;using_editor=1
;merge_command="emacs-merge %orig %new %merged"

;;Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
