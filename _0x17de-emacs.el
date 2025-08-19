;; -*- lexical-binding: t; -*-
;; To enable put the following as your .emacs file assuming
;; all contents were installed into ~/.emacs/_0x17de/
;;
;; (load "/path/to/_0x17de-emacs-config/_0x17de-emacs")

(setq initial-scratch-message nil
      ring-bell-function #'ignore
      byte-compile-warnings '(not lexical-binding))

(defvar _0x17de/load-path (file-name-directory (or load-file-name buffer-file-name))
  "Path of the _0x17de emacs config")
(when (not _0x17de/load-path)
  (error "Cannot determine configuration path of _0x17de emacs-config: both load-file-name and buffer-file-name are nil"))

(defun _0x17de/load (paths)
  "Load files relative to this emacs config."
  (when (not (listp paths))
    (setq paths (list paths)))
  (dolist (path paths)
    (condition-case err
        (load (expand-file-name path _0x17de/load-path))
      (error (message "Failed to load _0x17de-config sub-library at %S: %S" path err)))))

(_0x17de/load
 '("./init/custom.el"
   "./init/speedup.el"
   "./init/package.el"
   "./init/encoding.el"
   "./init/gui.el"
   "./init/exwm.el"))

(_0x17de/load
 '("./utils/exec-path-from-shell"
   "./utils/minibuffer"
   "./utils/indention"
   "./utils/replace-with-shell"
   "./utils/misc"
   "./utils/bolt"
   "./utils/vterm"
   "./utils/modeline"
   "./utils/multiple-cursors"
   "./utils/extended-command"))

(_0x17de/load
 '("./langs/common"
   "./langs/ansible"
   "./langs/plantuml"
   "./langs/lisp"
   "./langs/java"
   "./langs/go"
   "./langs/c"
   "./langs/python"
   "./langs/rust"
   "./langs/org"
   "./langs/xml"
   "./langs/misc"))

(_0x17de/load "./langs/latex")

(_0x17de/load
 '("./ext/tex-switch-quotes/tex-switch-quotes"
   "./ext/misc/hl-line+"))
