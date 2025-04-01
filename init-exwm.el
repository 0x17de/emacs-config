;; See https://github.com/ch11ng/exwm/wiki
;; https://github.com/ch11ng/exwm/wiki/Configuration-Example
(defcustom _0x17de/use-exwm nil
  "Non-nil means EXWM will be enabled.
When this option is enabled, EXWM will be loaded and configured
as the window manager for this session."
  :type 'boolean
  :group '_0x17de)
(when _0x17de/use-exwm
  (setq mouse-autoselect-window nil
        focus-follows-mouse nil)
  ;;(ido-mode 1)
  (use-package exwm
    :config
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    ;;(require 'exwm-config)
    ;;(exwm-config-ido)
    ;;(exwm-config-default)
    ;;(add-hook 'exwm-randr-screen-change-hook
    ;;        (lambda ()
    ;;          (start-process-shell-command
    ;;           "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
    ;;(use-package exwm-systemtray)
    (setq exwm-workspace-number 10)
    ;;(setq mouse-autoselect-window nil)
    (global-set-key [mode-line mouse-2] 'exwm-layout-toggle-fullscreen)
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-class-name))))
    (add-hook 'exwm-update-title-hook
              (lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-title))))
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            [s-o] . (lambda ()
  		    (interactive)
  		    (start-process "" nil "/usr/bin/slock"))))
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
    (exwm-randr-enable)
    (exwm-systemtray-enable)
    (exwm-enable)))
