(use-package org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c .") 'org-time-stamp))))
;;(use-package ox-search
;;  :ensure nil
;;  :hook (org-mode . ox-search-mode))
