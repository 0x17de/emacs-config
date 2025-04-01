(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq-default
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-verbose t)
