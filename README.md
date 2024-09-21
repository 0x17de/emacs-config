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
- Python: using "pyright" + flycheck; Requires `pyright` (I recommend to install with `pipx`)
- Rust: using racer + company-mode + flycheck
- LaTeX: usinx AUCTeX + compile on save minor mode "latex-compile-on-save-mode"
- golang: calls gofmt/goimports on save; run `M-x ox-install-go-dependencies`
- Java: using lsp-java. run `M-x lsp-install-server RET jdtls RET` to install the language server

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

; INFO: Your custom config can be either here or inside ~/.emacs.d/_0x17de/custom.el

(load "_0x17de-emacs")

; optional for hugo blogging
(setq easy-hugo-basedir "~/hugo/0x17blog")
```

For simplified starting there are scripts inside the bin directory. Create a symbolic link inside `/usr/local/bin/` pointing to `~/.emacs.d/_0x17de/bin/ew` - this is how i start a graphical emacs session while using the daemon functionality (to keep emacs running and all buffers loaded even if all windows are closed). See `M-x kill-emacs` and start emacs using `emacs --debug-init` while fiddling with the configuration.

Dependencies will be installed automatically on the first run via `use-package`.

Things to run once the setup is finished to have some C++ completion:
```
M-x irony-install-server
```

For golang support you might want to install at least some of the following packages into your `$GOPATH`. Also add the `$GOPATH` to your PATH and `'exec-path` like done for rust (cargo) above.
```
go get golang.org/x/tools/cmd/goimports
go get github.com/godoctor/godoctor
go get -u github.com/nsf/gocode
go get golang.org/x/tools/cmd/guru
go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
go get github.com/rogpeppe/godef
go get golang.org/x/tools/cmd/godoc
go get github.com/zmb3/gogetdoc
go get -u github.com/golang/dep/cmd/dep
```

## My favorite hotkeys

| Hotkey       | Description                                                                             |
| ------------ | --------------------------------------------------------------------------------------- |
| `C-M-z \|`   | multiple cursors: select all instances of the selected region                           |
| `C-M-z C-c`  | multiple cursors: with having multiple rows selected, place a cursor in every row       |
| `C-M-z >`    | multiple cursors: add a cursor at the next section matching the selection               |
| `C-M-z <`    | multiple cursors: add a cursor at the previous section matching the selection           |
| `M-<mouse1>` | multiple cursors: add/remove a cursor at the pointer location                           |
| `C-M-S-y`    | show snippets for current mode                                                          |
| `C-M-S-q`    | close current buffer without asking                                                     |
| `C-M-S-w`    | close current buffer and window without asking                                          |
| `C-M-x`      | The original M-x since `smex` is used to simplify finding commands                      |
| `C-M-\|`     | Pipe region through shell command and replace with result                               |
| `S-<tab>`    | A real tab character since `<tab>` is autoindent                                        |
| `<f1>`       | For some languages like python or c++ this will search for a documentation entry        |
| `<f5>`       | Generic compile command like for cmake projects                                         |
| `<f6>`       | Cross compile command for cmake projects overwrite cross-compile-command in .emacs file |
