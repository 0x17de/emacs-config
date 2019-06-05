# My emacs configuration

## Things that work

### General
- Easy installation of the configuration (packages are automatically installed via use-package)
- Autoindentation
- Git using "magit"
- Various modes to enhance the experience
- Snippets (yasnippet; userdefined ones can be put into ~/.emacs.d/snippets)
- Terminal support using "multi-term"
- Edit multiple text sections with multiple cursors

### Autocompletions and syntax check

Place a file `.dir-locals.el` into the project's root for some autocompletions like for C++ to correctly detect the project root.

- C++: using irony & syntax via clang + flycheck; working well together with cmake projects while always tring to use `$PROJECT_ROOT/build` instead of a directory inside /tmp/ for builds (you might want to call `M-x cmake-ide-run-cmake` if something looks messed up). The compile command (`<f5>`) also supports ninja based cmake projects.
- Python: using "jedi" + flycheck
- Rust: using racer + company-mode + flycheck
- LaTeX: usinx AUCTeX + compile on save minor mode "latex-compile-on-save-mode"
- golang: calls gofmt/goimports on save

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
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.cargo/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))

(add-to-list 'load-path "~/.emacs.d/_0x17de/")

; INFO: Be sure to put your custom-set-variables section
;       here - before the next load command - to allow
;       custom-safe-themes to take effect!

(load "_0x17de-emacs")

; optional for hugo blogging
(setq easy-hugo-basedir "~/hugo/0x17blog")
```

Dependencies will be installed automatically on the first run.

Things to run once the setup is finished to have some C++ completion:
```
M-x irony-install-server
```

## My favorite hotkeys

| Hotkey      | Description                                                                       |
| ----------- | --------------------------------------------------------------------------------- |
| `C-M-z \|`  | multiple cursors: select all instances of the selected region                     |
| `C-M-z C-c` | multiple cursors: with having multiple rows selected, place a cursor in every row |
| `C-M-z >`   | multiple cursors: add a cursor at the next section matching the selection         |
| `C-M-z <`   | multiple cursors: add a cursor at the previous section matching the selection     |
| `C-M-S-q`   | close current buffer without asking                                               |
| `C-M-x`     | The original M-x since `smex` is used to simplify finding commands                |
| `S-<tab>`   | A real tab character since `<tab>` is autoindent                                  |
| `<f1>`      | For some languages like python or c++ this will search for a documentation entry  |
| `<f5>`      | Generic compile command like for cmake projects                                   |
