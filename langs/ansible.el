;; -*- lexical-binding: t; -*-
(use-package ansible
  :defer t
  :bind
  (:map ansible-key-map
        ([f1] . 'ansible-doc)
        ([tab] . 'company-indent-or-complete-common))
  :hook (ansible-mode . (lambda ()
                          (add-to-list 'company-backends 'company-ansible)
                          (company-mode t))))
(use-package company-ansible
  :defer t)
