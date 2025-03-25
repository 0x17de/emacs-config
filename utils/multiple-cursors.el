(defun ox/add-multiple-cursors-to-non-empty-lines (start end)
  "Add a cursor in each non-empty line of selection"
  (interactive "r")
  (require 'multiple-cursors)
  (let ((original-point (point)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((line-begin (line-beginning-position))
              (line-end (line-end-position)))
          (unless (or (eq line-begin line-end)
                      (and (>= original-point line-begin)
                           (<= original-point line-end)))
            (mc/mark-next-like-this 1))
          (goto-char (min (1+ line-end) end))))
      (mc/maybe-multiple-cursors-mode))))
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-M-z C-c" . mc/edit-lines)
   ("C-M-z C-M-c" . ox/add-multiple-cursors-to-non-empty-lines)
   ("C-M-z >" . mc/mark-next-like-this)
   ("C-M-z <" . mc/mark-previous-like-this)
   ("C-M-z |" . mc/mark-all-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))
