(defvar overlay-test--overlays '())

(defun overlay-test--mark ()
  "mark overlay with font"
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end))
        (buf (current-buffer))
        (my-overlay))
    (setq my-overlay (make-overlay beg end buf))
    (add-to-list 'overlay-test--overlays my-overlay)
    (overlay-put my-overlay 'face compilation-error-face)))
  
(defun overlay-test--reset ()
  "mark overlay with font"
  (interactive)
  (dolist (e overlay-test--overlays)
    (delete-overlay e))
  (setq overlay-test--overlays '()))
