(require 'package)
;(setq-default load-prefer-newer t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;(setq-default package-enable-at-startup nil)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package t))
(require 'use-package)
(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-verbose t)
(use-package use-package-ensure-system-package)
