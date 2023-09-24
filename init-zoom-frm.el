(add-to-list 'load-path "~/.emacs.d/_0x17de/ext/zoom-frm/")

(defun download-and-load-zoom-frm ()
  "Download and load the zoom-frm.el from EmacsWiki if it does not exist, else just load it."
  (let* ((dir "ext/zoom-frm/")
         (file-url-prefix "https://www.emacswiki.org/emacs/download/")
         (file-name-zoom-frm "zoom-frm.el")
         (file-name-frame-cmds "frame-cmds.el")
         (file-name-frame-fns "frame-fns.el"))

    ;; Check if the directory exists, if not create it
    (unless (file-exists-p dir)
      (make-directory dir t))

    ;; Check if the file exists
    (dolist (dep-name (list file-name-zoom-frm file-name-frame-cmds file-name-frame-fns))
      (let* ((file-name (concat dir dep-name))
             (file-url (concat file-url-prefix dep-name)))
        (if (file-exists-p file-name)
            (message (concat "File " file-name " already exists."))
          (progn
            (message (concat "Downloading " file-name "..."))
            (condition-case err
                (url-copy-file file-url file-name t)
              (error
               (message "Failed to download zoom-frm.el: %s" (error-message-string err))
               (cl-return-from download-and-load-zoom-frm nil)))))))

    ;; Load the file
    (load file-name-zoom-frm)
    (message "zoom-frm.el loaded successfully!")))
