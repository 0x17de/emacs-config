(let ((customization-file (expand-file-name "custom.el" (expand-file-name "_0x17de" user-emacs-directory))))
  (if (file-exists-p customization-file)
      (progn
        (message "Using custom file: %s" customization-file)
        (setq custom-file customization-file)
        (load custom-file 'noerror))
    (progn
      (message "Using default custom file location: %s" custom-file))))

