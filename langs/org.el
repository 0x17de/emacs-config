(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
        ("C-c ." . 'org-time-stamp))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  (setq org-confirm-babel-evaluate nil
        org-todo-keywords '((sequence "TODO" "WAITING" "DOING" "|" "DONE" "CANCELLED"))))

(use-package org-journal
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
  :after org
  :config (org-node-cache-mode))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)))

(defun org-babel-execute:nushell (body params)
  "Execute a block of nushell code"
  (let* ((cmd (or (cdr (assoc :cmd params)) "nu"))
         (args (or (cdr (assoc :args params)) "-c"))
         (code (shell-quote-argument (org-babel-chomp body)))
         (command (format "%s %s %s" cmd args code)))
    (org-babel-eval command "")))

(defun org-babel-execute:jinja (body params)
  "Execute a jinja2 template"
  (let* ((script (shell-quote-argument "import sys, jinja2 as j2; print(j2.Template(sys.argv[1]).render())"))
         (code (shell-quote-argument (org-babel-chomp body)))
         (command (format "python3 -c %s %s" script code)))
    (org-babel-eval command "")))
