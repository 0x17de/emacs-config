;; -*- lexical-binding: t; -*-
(use-package ansible
  :defer t
  :bind
  (:map ansible-key-map
        ([f1] . 'ansible-doc))
  :hook (ansible-mode . (lambda ()
                          ;; ansible-mode is a minor mode on yaml-mode, which
                          ;; global-corfu-modes leaves out.
                          (add-hook 'completion-at-point-functions
                                    (cape-company-to-capf #'company-ansible) nil t)
                          (corfu-mode t))))
(use-package company-ansible
  :defer t
  :commands company-ansible)
