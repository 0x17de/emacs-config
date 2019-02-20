# My emacs configuration

## Things that work

General:
- Easy installation of the configuration
- Autoindentation
- Git using "magit"
- Various modes to enhance the experience
- Snippets (yasnippet)
- Terminal support using "multi-term"
- Edit multiple text sections with multiple cursors

Autocompletions and syntax check:
- C++: using irony & syntax via clang + flycheck
- Python: using "jedi" + flycheck
- Rust: using racer + company-mode + flycheck
- LaTeX: usinx AUCTeX + compile on save minor mode "latex-compile-on-save-mode"

## Setup

Clone the repository into `~/.emacs.d/_0x17de/` and install the submodules:
```
git clone --recursive https://github.com/0x17de/emacs-config/ ~/.emacs.d/_0x17de/
```

After that the following should be your .emacs file:

```
(package-initialize)

; optional org mode encryption settings
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "123...GPGID...DEF")

; optional for rust configuration
(setenv "PATH" (concat (getenv "PATH") ":/home/USER/.cargo/bin"))
(add-to-list 'exec-path "/home/USER/.cargo/bin")

(add-to-list 'load-path "~/.emacs.d/_0x17de/")
(load "_0x17de-emacs")

; optional for hugo blogging
(setq easy-hugo-basedir "~/hugo/0x17blog")
```

Dependencies will be installed automatically on the first run.

Things to run once the setup is finished to have some C++ completion:
```
M-x irony-install-server
```
