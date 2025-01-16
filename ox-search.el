;;+begin-search: :match "begin-search"

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
                 (match-string 1 lin)e))))
    (when args
      (condition-case nil
          (let ((match (plist-get (car (read-from-string (concat "(" args ")"))) :match)))
            match)
        (error nil)))))

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
                  (push (format "- %s (line %d)"
                                line-content
                                line-num)
                        results))))))
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
