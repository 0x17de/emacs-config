;; Just a little helper minor mode to compile latex files on save

(defgroup latex-compile-on-save nil
  "Refreshes the LaTeX preview on save"
  :group 'convenience
  :prefix 'latex-compile-on-save
  :link '(function-link latex-compile-on-save-mode))

(defun latex-compile-on-save--compile ()
  "Calls the compile function"
  (TeX-command-run-all nil))

;;;###autoload
(define-minor-mode latex-compile-on-save-mode
  "Refresh the preview on save"
  :lighter " LTeXcos"
  :group latex-compile-on-save
  (if latex-compile-on-save-mode
      (add-hook 'after-save-hook 'latex-compile-on-save--compile nil t)
    (remove-hook 'after-save-hook 'latex-compile-on-save--compile t)))

(provide 'latex-compile-on-save)
