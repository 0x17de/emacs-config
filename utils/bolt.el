(require 'cl-lib)
(require 'widget)
(require 'wid-edit)
(require 'vterm)

(defcustom _0x17de/bolt-command-path "~/.local/share/gem/ruby/3.3.0/bin/bolt"
  "Path to the bolt binary."
  :group '_0x17de
  :type 'file)
(defcustom _0x17de/bolt-repo-path "~/git/bolt-control"
  "Path to the bolt binary."
  :group '_0x17de
  :type 'directory)

(defvar _0x17de/bolt-finish-fun nil
  "The callback when bolt execution finished.")
(make-variable-buffer-local '_0x17de/bolt-finish-fun)

(defun _0x17de/bolt--erase-buffer (buffer)
  "Erase the buffer."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (dolist (widget widget-field-list)
        (widget-delete widget))
      (kill-all-local-variables)
      (erase-buffer)
      (remove-overlays)
      (setq buffer-read-only nil)))
  buffer)

(defun _0x17de/bolt-run-shell (bolt-buffer-name bolt-args)
  (let* ((buffer (let ((default-directory _0x17de/bolt-repo-path)
                       (vterm-shell "/bin/sh")
                       (vterm-buffer-name bolt-buffer-name)
                       (vterm-exit-functions nil)
                       (vterm-kill-buffer-on-exit nil))
                   (vterm))))
    (with-current-buffer buffer
      (let* ((cmd (string-join (cl-list* "BOLT_GEM=1" _0x17de/bolt-command-path (mapcar 'shell-quote-argument (cl-list* bolt-args))) " ")))
        (make-variable-buffer-local 'vterm-kill-buffer-on-exit)
        (setq vterm-kill-buffer-on-exit nil)
        (vterm-send-string "clear; ")
        (vterm-send-string cmd)
        (vterm-send-string "; exit\n")))
    (switch-to-buffer buffer)))

(defun _0x17de/bolt-run (bolt-buffer-name bolt-args on-finish-fun)
  "Run bolt with parameters."
  (let* ((buffer (_0x17de/bolt--erase-buffer (get-buffer-create bolt-buffer-name)))
         (bolt-control-repo _0x17de/bolt-repo-path)
         (default-directory bolt-control-repo)
         (process-environment (cl-list* "BOLT_GEM=1" process-environment))
         (proc (apply #'start-process
                                   bolt-buffer-name
                                   buffer
                                   _0x17de/bolt-command-path bolt-args)))
    (with-current-buffer (process-buffer proc)
      (setq _0x17de/bolt-finish-fun on-finish-fun))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (let ((buf (process-buffer proc)))
         (when (string-match-p "finished" event)
           (with-current-buffer buf
             (ansi-color-filter-region (point-min) (point-max))
             (goto-char (point-min))
             (funcall _0x17de/bolt-finish-fun buf))))))))

(defun _0x17de/bolt--line-string ()
  "Return the current line string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun _0x17de/bolt-parse-plan-from-plan-show (buffer)
  "Parse a single plan's details from a bolt plan show execution buffer."
  (let ((res '())
        (start-pos nil)
        (end-pos nil)
        (desc nil))
    (with-current-buffer buffer
      (re-search-forward "\n")
      ;; parse description
      (setq start-pos (point))
      (re-search-forward "\n\n" nil t)
      (setq end-pos (point))
      (add-to-list 'res (cons 'desc (buffer-substring-no-properties start-pos end-pos)))
      ;; parse parameters
      (when (re-search-forward "Parameters\n" nil t)
        (let (line
              start-pos
              end-pos
              (params '())
              param-name
              param-type
              param-desc)
          (cl-loop
           do (setq line (_0x17de/bolt--line-string))
           while (string-match "^  \\([^ ]+\\) *\\(.*\\)$" line)
           do (progn
                (setq param-name (match-string 1 line)
                      param-type (match-string 2 line))
                (forward-line)
                (setq start-pos (point))
                (setq line (_0x17de/bolt--line-string))
                (if (string-empty-p line)
                    (progn
                      (forward-line)
                      (add-to-list 'params (list param-name :name param-name :type param-type)))
                  (progn
                    (re-search-forward "\n\n" nil t)
                    (setq end-pos (point))
                    (setq param-desc (string-trim-right
                                      (buffer-substring-no-properties start-pos end-pos)))
                    (add-to-list 'params (list param-name :name param-name :type param-type :desc param-desc :value ""))))))
          (add-to-list 'res (cons 'params params)))))
    res))


(defun _0x17de/bolt-parse-plans-from-plan-show (buffer)
  "Parse plans from a bolt plan show execution buffer."
  (let ((plans '())
        (line nil))
    (with-current-buffer buffer
      (re-search-forward "Plans\n")
      (cl-loop do (setq line (_0x17de/bolt--line-string))
               while (string-match "^  \\([^ ]+\\) *\\(.*\\)$" line)
               do (progn
                    (push
                     (cons (match-string 1 line) (match-string 2 line))
                     plans)
                    (forward-line 1))))
    plans))

(defun _0x17de/bolt-plan-run (bolt-plan-name bolt-params)
  "Run a bolt plan with given parameters."
  (let* ((bolt-buffer-name (format "*bolt-plan-run--%s*" bolt-plan-name))
         (bolt-args (delq nil
                          (mapcar
                           (lambda (param)
                             (let ((value (plist-get (cdr param) :value)))
                               (if (string-empty-p value)
                                   nil
                                 (concat (plist-get (cdr param) :name)
                                         "="
                                         (plist-get (cdr param) :value)))))
                           bolt-params))))
    (_0x17de/bolt-run-shell
     bolt-buffer-name
     `("plan" "run" ,bolt-plan-name ,@bolt-args))))

(defun _0x17de/bolt-plan (bolt-plan-name)
  "Display a bolt plan."
  (interactive)
  (_0x17de/bolt-run
   (format "*bolt-plan-show--%s*" bolt-plan-name)
   `("plan" "show" ,bolt-plan-name)
   `(lambda (plan-show-buffer)
      (let* ((parsed (_0x17de/bolt-parse-plan-from-plan-show plan-show-buffer))
             (bolt-buffer-name (format "*bolt-plan--%s*" ,bolt-plan-name))
             (buffer (_0x17de/bolt--erase-buffer (get-buffer-create bolt-buffer-name)))
             (desc (alist-get 'desc parsed)))
        (with-current-buffer buffer
          (insert "Plan " ,bolt-plan-name "\n\n")
          (insert "Description\n" desc)
          (make-variable-buffer-local 'params)
          (setq params (alist-get 'params parsed))
          (when params
            (insert "Parameters\n")
            (dolist (param params)
              (let* ((param-name (car param))
                     (param-data (cdr param))
                     (param-type (plist-get param-data :type))
                     (param-desc (plist-get param-data :desc)))
                (insert "  " param-name " " param-type "\n")
                (insert "    ")
                (let ((widget (widget-create
                               'editable-field
                               :size 40
                               :format "%v"
                               :notify (lambda (widget &rest ignore)
                                         (let ((param (widget-get widget :param))
                                               (value (widget-value widget)))
                                           (setf (cdr param) (plist-put (cdr param) :value value)))))))
                  (widget-put widget :param param))
                (insert "\n")
                (if param-desc
                    (insert (string-trim-right param-desc) "\n\n")
                  (insert "\n")))))
          (insert "\n\n  ")
          (widget-create 'push-button
                         :notify (lambda (widget &rest ignore)
                                   (let ((bolt-params (widget-get widget :params)))
                                     (_0x17de/bolt-plan-run ,bolt-plan-name bolt-params)))
                         :params params
                         "Run!")
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map widget-keymap)
            (define-key map (kbd "g") (lambda () (interactive)
                                        (_0x17de/bolt-plan ,bolt-plan-name)))
            (use-local-map map))
          (widget-setup)
          (goto-char (point-min))
          (widget-forward 1))
        (switch-to-buffer buffer)))))

(defun _0x17de/bolt--editable-field-overlay-maintain (overlay after begin end &optional length)
  "Maintain the overlay covering the editable field."
  (when after
    (let ((widget (overlay-get overlay 'widget)))
      (move-overlay overlay
                    (widget-field-start widget)
                    (widget-field-end widget)))))

(defun _0x17de/bolt ()
  "Start the bolt interface."
  (interactive)
  (_0x17de/bolt-run
   "*bolt-plan-show*"
   '("plan" "show")
   (lambda (plan-show-buffer)
     (let* ((plans (_0x17de/bolt-parse-plans-from-plan-show plan-show-buffer))
            (buffer (_0x17de/bolt--erase-buffer (get-buffer-create "*bolt*"))))
       (with-current-buffer buffer
         (setq truncate-lines t)
         (insert "Plans\n")
         (dolist (plan plans)
           (let ((name (car plan))
                 (doc (cdr plan)))
             (insert "  ")
             (insert-text-button
              name
              'help-echo (and (not (string-empty-p doc)) doc)
              'action `(lambda (_) (_0x17de/bolt-plan ,name)))
             (when (and doc (not (string-empty-p doc)))
               (insert ": " doc))
             (insert "\n"))))
       (display-buffer buffer)))))
