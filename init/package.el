;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Without this, `use-package-always-ensure' fails for every package on a
;; machine that has never fetched the archives. Read the local cache first:
;; nothing has loaded it yet at this point, so checking the variable alone
;; would hit the network on every startup.
(unless package-archive-contents
  (package-read-all-archive-contents))
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq-default use-package-always-ensure t)

(let* ((base-dir (file-name-parent-directory (file-name-directory (or load-file-name buffer-file-name))))
       (packages-dir (expand-file-name "packages" base-dir)))
  (when (file-directory-p packages-dir)
    (add-to-list 'load-path packages-dir)))
