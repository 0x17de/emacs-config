(let ((customization-file (expand-file-name "custom.el"
                                            (file-name-directory
                                             (directory-file-name
                                              (file-name-directory (or load-file-name
                                                                       buffer-file-name)))))))
  (if (file-exists-p customization-file)
      (progn
        (message "Using custom file: %s" customization-file)
        (setq custom-file customization-file)
        (load custom-file 'noerror))
    (progn
      (message "Using default custom file location: %s" custom-file))))

(defgroup _0x17de nil
  "Configuration options for _0x17de Emacs setup."
  :prefix "_0x17de/"
  :group 'convenience)
