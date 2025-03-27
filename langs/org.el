(use-package org
  :ensure nil
  :defer t
  :bind
  (:map org-mode-map
        ("C-c ." . 'org-time-stamp))
  :hook
  (org-mode . _0x17de/org-setup-deadline-highlighting)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  (setq org-confirm-babel-evaluate nil
        org-todo-keywords '((sequence "TODO" "WAITING" "DOING" "|" "DONE" "CANCELLED"))
        org-modern-todo-faces '(("TODO" :foreground "white" :background "darkgreen" :weight bold)
                                ("DOING" :foreground "white" :background "orange" :weight bold)
                                ("WAITING" :foreground "white" :background "blue"))))

(use-package org-journal
  :defer t
  :bind
  (("C-M-S-j" . 'org-journal-new-entry))
  :init
  (setq org-journal-dir "~/org/journal"
        org-journal-date-format "%Y-%m-%d"
        org-journal-enable-agenda-integration t))

;;(use-package ox-search
;;  :ensure nil
;;  :hook (org-mode . ox-search-mode))

(use-package org-node
  :defer t
  :after org
  :config (org-node-cache-mode))

(use-package org-modern
  :defer t
  :hook ((org-mode . org-modern-mode)))

(defun org-babel-nushell--list-includes (params)
  "Extract values from all :include keys in PARAMS alist."
  (let ((result nil))
    (dolist (pair params result)
      (when (and (consp pair)
                 (eq (car pair) :include))
        (push (cdr pair) result)))))

(defun org-babel-nushell--read-includes (params)
  "Read all includes"
  (let ((result '()))
    (dolist (name (org-babel-nushell--list-includes params))
      (let ((block-position (org-babel-find-named-block name)))
        (when block-position
          (save-excursion
            (goto-char block-position)
            (let ((data (org-babel-get-src-block-info)))
              (when data
                (push (nth 1 data) result)))))))
    (string-join (nreverse result) "\n")))

(defun org-babel-nushell-var-to-nushell (params)
  (format "let $%s = %s" (car params) (json-encode (cdr params))))

(defcustom org-babel-nushell-eval-shell-file-name "/bin/sh"
  "Shell executable used by `org-babel-eval' to execute Nushell source blocks.
This shell is used to execute the Nushell command."
  :type 'string
  :group '_0x17de)

(defun org-babel-execute:nushell (body params)
  "Execute a block of nushell code"
  (let* ((shell-file-name org-babel-nushell-eval-shell-file-name)
         (cmd (or (cdr (assoc :cmd params)) "nu"))
         (args (or (cdr (assoc :args params)) "-c"))
         (vars (mapcar
                'org-babel-nushell-var-to-nushell
                (org-babel--get-vars params)))
         (included-code (org-babel-nushell--read-includes params))
         (code (string-join (list (or included-code "") (org-babel-chomp body)) "\n"))
         (full-code (if vars
                        (concat (string-join vars "\n") "\n" code)
                      code))
         (command (format "%s %s %s" cmd args (shell-quote-argument full-code))))
    (org-babel-eval command "")))

(defun org-babel-execute:jinja (body params)
  "Execute a jinja2 template"
  (let* ((script (shell-quote-argument "import sys, jinja2 as j2; print(j2.Template(sys.argv[1]).render())"))
         (code (shell-quote-argument (org-babel-chomp body)))
         (command (format "python3 -c %s %s" script code)))
    (org-babel-eval command "")))

(defface _0x17de/org-deadline-highlight
  '((t . (:background "#4c1f24")))
  "Highlight face for today's deadlines")

(defun _0x17de/org-highlight-todays-deadlines ()
  "Highlight headlines with a deadline of today in Org mode."
  (interactive)
  (save-excursion
    (remove-overlays (point-min) (point-max) 'face '_0x17de/org-deadline-highlight)
    (goto-char (point-min))
    (let ((today (calendar-current-date))
          (current-headline nil)
          (current-headline-start nil)
          (current-headline-end nil))
      (while (re-search-forward "^\\*+ \\(.*\\)$\\|DEADLINE: <\\([^>]+\\)>" nil t)
        (cond
         ((match-string 1)
          (setq current-headline (match-string-no-properties 1)
                current-headline-start (match-beginning 0)
                current-headline-end (line-end-position)))
         ((match-string 2)
          (ignore-errors
            (let* ((current-deadline (match-string-no-properties 2))
                   (deadline-parsed (org-parse-time-string current-deadline))
                   (deadline-date-abs (calendar-absolute-from-gregorian
                                       (list (nth 4 deadline-parsed)
                                             (nth 3 deadline-parsed)
                                             (nth 5 deadline-parsed))))
                   (today-date-abs (calendar-absolute-from-gregorian today))
                   (days-diff (- deadline-date-abs today-date-abs)))
              (when (eq days-diff 0)
                (let ((ov (make-overlay current-headline-start current-headline-end)))
                  (overlay-put ov 'face '_0x17de/org-deadline-highlight)))))))))))

(defun _0x17de/org-setup-deadline-highlighting ()
  (when (derived-mode-p 'org-mode)
    (add-hook 'after-save-hook #'_0x17de/org-highlight-todays-deadlines nil t)
    (add-hook 'after-change-functions (lambda (&rest _) (_0x17de/org-highlight-todays-deadlines)) nil t)
    (_0x17de/org-highlight-todays-deadlines))) ;; run once initially
