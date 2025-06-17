;; -*- lexical-binding: t; -*-
(defun _0x17de/add-multiple-cursors-to-non-empty-lines (start end)
  "Add a cursor in each non-empty line of selection"
  (interactive "r")
  (save-excursion
    (let ((start (min (mark) (point)))
          (end (max (mark) (point))))
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
        (unless (string-match-p "^\s*$" (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
          (mc/create-fake-cursor-at-point))
        (forward-line 1))))
  (mc/maybe-multiple-cursors-mode))
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-M-z C-c" . mc/edit-lines)
   ("C-M-z C-M-c" . _0x17de/add-multiple-cursors-to-non-empty-lines)
   ("C-M-z >" . mc/mark-next-like-this)
   ("C-M-z <" . mc/mark-previous-like-this)
   ("C-M-z |" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))
