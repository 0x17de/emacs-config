(defcustom _0x17de/plantuml:enable t
  "Whether the plantuml mode is enabled."
  :group '_0x17de
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-to-list 'auto-mode-alist '("\\.puml\\'" . _0x17de/plantuml-mode))
           (setq auto-mode-alist
                 (delete '("\\.puml\\'" . _0x17de/plantuml-mode) auto-mode-alist)))))

(defcustom _0x17de/plantuml-jar-file "~/opt/plantuml/plantuml.jar"
  "The path to the plantuml jar file."
  :group '_0x17de
  :type 'file)

(defun _0x17de/plantuml-generate-png ()
  "Generate a PNG from the current plantuml file."
  (when (and _0x17de/plantuml-jar-file (file-exists-p _0x17de/plantuml-jar-file))
    (let ((cmd (format "java -jar %s -tpng %s"
                       (shell-quote-argument (expand-file-name _0x17de/plantuml-jar-file))
                       (shell-quote-argument buffer-file-name))))
      (shell-command cmd))))

(defun _0x17de/plantuml-preview ()
  "Display the PNG image for a plantuml file."
  (let* ((png-file (concat (file-name-sans-extension buffer-file-name) ".png"))
         (buf-name "*plantuml-preview*"))
    (when (file-exists-p png-file)
      (let ((preview-buffer (get-buffer-create buf-name)))
        (with-current-buffer preview-buffer
          (setq buffer-read-only nil)
          (erase-buffer)

          (use-local-map (make-sparse-keymap))
          (local-set-key (kbd "q") 'quit-window)
          
          (clear-image-cache png-file)
          (insert-image (create-image png-file))
          (setq header-line-format "Press 'q' to close.")
          (setq buffer-read-only t)
          (unless (get-buffer-window preview-buffer)
            (display-buffer preview-buffer)))))))

(defun _0x17de/plantuml-save-hook ()
  "Hook to run after saving a plantuml file."
  (when (string= (file-name-extension buffer-file-name) "puml")
    (_0x17de/plantuml-generate-png)
    (_0x17de/plantuml-preview)))

(define-derived-mode _0x17de/plantuml-mode prog-mode "PUML"
  "Major mode for editing plantuml files."
  :group '_0x17de
  (add-hook 'after-save-hook '_0x17de/plantuml-save-hook nil t))

(provide '_0x17de/plantuml-mode)
