;;+begin-search: :match "begin-search"
(require 'button)  ; Add this to ensure button library is loaded

(defun ox-parse-search-line ()
  "Parse #+begin-search: or ;;+begin-search: or //+begin-search: line as a plist."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (args (cond
                ((string-match "^#\\+begin-search:\\(.*\\)" line)
                 (match-string 1 line))
                ((string-match "^;;\\+begin-search:\\(.*\\)" line)
                 (match-string 1 line))
                ((string-match "^//\\+begin-search:\\(.*\\)" line)
                 (match-string 1 line)))))
    (when args
      (condition-case nil
          (let ((match (plist-get (car (read-from-string (concat "(" args ")"))) :match)))
            match)
        (error nil)))))

(defmacro ox--with-mode-save (&rest body)
  "Execute save/restore if in appropriate mode."
  `(if (derived-mode-p 'org-mode)
       (org-fold-core-save-visibility ,@body)
     (progn ,@body)))

(defun ox-goto-line (line-num)
  "Jump to the specified line number while preserving search results view."
  (interactive "nGoto line: ")
  (let ((target-line (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (forward-line (1- line-num))
                         (point)))))
    (push-mark)
    (goto-char target-line)
    (pulse-momentary-highlight-one-line (point))))

(define-button-type 'ox-line-button
  'action (lambda (button)
            (ox-goto-line (button-get button 'target-line)))
  'follow-link t
  'help-echo "Click to jump to this line")

(defun ox-make-line-button (line-num)
  "Create a button for jumping to LINE-NUM."
  (with-temp-buffer
    (insert (format "(line %s)" line-num))
    (make-text-button (point-min) (point-max)
                      'type 'ox-line-button
                      'target-line line-num)
    (buffer-string)))

(defun ox-update-search-at-point ()
  "Update the search results at point."
  (interactive)
  (let* ((search-line-start (line-beginning-position))
         (search-line-end (line-end-position))
         (pattern (ox-parse-search-line)))
    (when pattern
      (let ((ov (make-overlay search-line-end search-line-end))
            (matches (make-hash-table :test 'equal))  ; Use hash table to track unique lines
            (results '()))
        (save-excursion
          (ox--with-mode-save
           (goto-char (point-min))
           (while (re-search-forward pattern nil t)
             (unless (and (>= (point) search-line-start)
                          (<= (point) search-line-end))
               (let* ((line-num (line-number-at-pos))
                      (line-content (string-trim-left
                                     (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))
                 ;; Only add if we haven't seen this line number before
                 (unless (gethash line-num matches)
                   (puthash line-num t matches)
                   (push (format "- %s %s"
                                 line-content
                                 (ox-make-line-button line-num))
                         results)))))))
        (overlay-put ov 'after-string
                     (concat " [Update]\n"
                             (mapconcat 'identity (nreverse results) "\n")
                             "\n"))))))

(defun ox-update-all-searches ()
  "Update all search results in buffer."
  (save-excursion
    (goto-char (point-min))
    (remove-overlays (point-min) (point-max))
    (while (re-search-forward "^\\(?:#\\|;;\\|//\\)\\+begin-search:" nil t)
      (beginning-of-line)
      (when (ox-parse-search-line)
        (ox-update-search-at-point))
      (forward-line))))

(define-minor-mode ox-search-mode
  "Toggle real-time search results display in buffer."
  :lighter " OxSearch"
  :group 'ox-search
  :global nil
  (if ox-search-mode
      (progn
        (add-hook 'after-save-hook #'ox-update-all-searches nil t)
        (ox-update-all-searches))
    (progn
      (remove-hook 'after-save-hook #'ox-update-all-searches t)
      (remove-overlays (point-min) (point-max)))))

(provide 'ox-search)
