;; -*- lexical-binding: t; -*-
(defcustom _0x17de/modeline 'doom-modeline
  "The modeline to use.
Options are 'doom-modeline or 'smart-mode-line."
  :type '(choice
          (const :tag "Doom modeline" doom-modeline)
          (const :tag "Smart mode line" smart-mode-line))
  :group '_0x17de)
(cond
 ((eq _0x17de/modeline 'doom-modeline)
  (use-package doom-modeline
    :ensure t
    :init
    (doom-modeline-mode t)))
 ((eq _0x17de/modeline 'smart-mode-line)
  (use-package smart-mode-line
    :ensure t
    :config
    (sml/setup))))
