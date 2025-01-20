(defun add-theme-to-safe-list (theme)
  "Compute the SHA-256 hash of the THEME and add it to `custom-safe-themes'."
  (let* ((theme-file (locate-file (concat (symbol-name theme) "-theme.el")
                                 custom-theme-load-path))
         (hash (when theme-file
                 (with-temp-buffer
                   (insert-file-contents-literally theme-file)
                   (secure-hash 'sha256 (current-buffer))))))
    (when (and hash (not (member hash custom-safe-themes)))
      (add-to-list 'custom-safe-themes hash)
      (customize-save-variable 'custom-safe-themes custom-safe-themes))))
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (add-theme-to-safe-list 'sanityinc-tomorrow-eighties))
