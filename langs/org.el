(use-package org
  :ensure nil
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c .") 'org-time-stamp))))
(use-package org-journal
  :bind
  (("C-M-S-j" . 'org-journal-new-entry)))
;;(use-package ox-search
;;  :ensure nil
;;  :hook (org-mode . ox-search-mode))

(use-package org-node
  :after org
  :config (org-node-cache-mode))
