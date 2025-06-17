;; -*- lexical-binding: t; -*-
(defun replace-with-shell (posBegin posEnd command)
  "Pipe region through shell command and replace"
  (interactive (let (command)
		 (unless (mark)
		   (user-error "The mark is not set now, so there is no region"))
                 (setq command (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end) command)))
  (with-temp-message ""
    (let ((string (buffer-substring-no-properties posBegin posEnd))
          (buffer (current-buffer))
          (shell-output-exists (get-buffer "*Shell Command Output*")))
      (with-temp-buffer
        (insert string)
        (shell-command-on-region (point-min) (point-max) command (current-buffer) t)
        (setq string (buffer-substring-no-properties (point-min) (point-max))))
      (delete-region posBegin posEnd)
      (insert string)
      ;; If *Shell Command Output* wasn't opened before, kill it
      (unless shell-output-exists
        (let ((output-buffer (get-buffer "*Shell Command Output*")))
          (when output-buffer
            (kill-buffer output-buffer)))))))

(global-set-key (kbd "C-M-|") 'replace-with-shell)
