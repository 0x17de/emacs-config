(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(setq-default
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-verbose t)

(let* ((base-dir (file-name-parent-directory (file-name-directory (or load-file-name buffer-file-name))))
       (packages-dir (expand-file-name "packages" base-dir)))
  (when (file-directory-p packages-dir)
    (add-to-list 'load-path packages-dir)))
