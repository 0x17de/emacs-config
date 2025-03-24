# My Emacs Configuration

## Overview

A modular Emacs configuration with comprehensive language support, development tools, and UI enhancements. Designed for easy extension and maintenance, with particular focus on developer experience.

## Features

### General
- Easy installation of the configuration (packages are automatically installed via use-package)
- Performance optimizations for faster startup and responsiveness
- Frame and window management utilities (resize, zoom, tiling, etc.)
- Multiple cursor editing
- Snippets (yasnippet; user-defined ones can be put into ~/.emacs.d/snippets)
- Smart completion via company-mode and LSP
- Documentation lookup integration
- Many quality-of-life improvements for editing

### Development Tools
- Version control using Magit
- Language Server Protocol (LSP) support
- Syntax checking via Flycheck
- Code formatting and refactoring tools
- Comprehensive support for project management

### Language Support

Language support is primarily based on LSP and/or specialized modes:

- **C/C++**: irony-mode + company-irony + flycheck + CMake integration
- **Python**: lsp-pyright + company + flycheck
- **Go**: LSP (gopls) + comprehensive Go tooling
- **Rust**: LSP-based tools
- **LaTeX**: AUCTeX with compile-on-save functionality
- **Web**: HTML, CSS, JavaScript support
- **Org-mode**: Enhanced with org-modern, org-journal, and custom babel support
- **Misc**: Support for YAML, JSON, Docker, Ansible, and many others

## Setup

Clone the repository into `~/.emacs.d/_0x17de/` and install the submodules:
```
git clone --recursive https://github.com/0x17de/emacs-config/ ~/.emacs.d/_0x17de/
```

After that, create a `.emacs` file with the following content:

```elisp
(package-initialize)

; optional org mode encryption settings
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "123...GPGID...DEF")

; optional for rust configuration (if you need Rust)
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.cargo/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))

(add-to-list 'load-path "~/.emacs.d/_0x17de/")

; INFO: Your custom config can be either here or inside ~/.emacs.d/_0x17de/custom.el

(load "_0x17de-emacs")

; optional for hugo blogging
; (setq easy-hugo-basedir "~/hugo/0x17blog")
```

## First-Time Setup

LSP and language features will need additional setup:

### For C/C++ development
```
M-x irony-install-server
```

### For Go development
Run once to install Go language tools:
```
M-x ox-install-go-dependencies
```

### For Java development
Install the Java language server:
```
M-x lsp-install-server RET jdtls RET
```

## Customization

You can put your own customizations in either your `.emacs` file or in `~/.emacs.d/_0x17de/custom.el`, which will be automatically loaded if it exists.

## Hotkeys

| Hotkey          | Description                                                           |
|-----------------|-----------------------------------------------------------------------|
| `C-M-z \|`      | Multiple cursors: select all instances of the selected region         |
| `C-M-z C-c`     | Multiple cursors: place cursor in every line of selected region       |
| `C-M-z C-M-c`   | Multiple cursors: place cursor on non-empty lines of selected region  |
| `C-M-z >`       | Multiple cursors: add a cursor at the next matching region            |
| `C-M-z <`       | Multiple cursors: add a cursor at the previous matching region        |
| `M-<mouse1>`    | Multiple cursors: add/remove a cursor at the pointer location         |
| `C-M-S-y`       | Show snippets for current mode                                        |
| `C-M-S-q`       | Close current buffer without asking                                   |
| `C-M-S-w`       | Close current buffer and window without asking                        |
| `C-M-x`         | The original M-x since `smex` is used for command selection           |
| `C-M-\|`        | Pipe region through shell command and replace with result             |
| `S-<tab>`       | Insert a real tab character instead of spaces                         |
| `<f1>`          | Show documentation for item at point (context-dependent)              |
| `<f5>`          | Generic compile command (project-specific actions in some modes)      |
| `<tab>`         | Auto-indentation and completion (intelligent context handling)        |
| `C-x o`         | Smart navigation between windows/frames                               |
| `M-<arrows>`    | Move between frames                                                   |
| `C-M-<up/down>` | Resize frames                                                       |

## Language-Specific Features

### C/C++ Projects
Place a `.dir-locals.el` in the project root for project-specific configuration. For CMake projects, the system will try to use `$PROJECT_ROOT/build` for builds.

### Python Projects
Requires `pyright` to be installed (recommended via `pipx install pyright`).

### Go Projects
Automatically formats code with gofmt/goimports on save.

### LaTeX Projects
Use the minor mode `latex-compile-on-save-mode` to automatically compile on saving.

## Troubleshooting

If you encounter issues with the configuration:
1. Start Emacs with `emacs --debug-init` to see initialization errors
2. Check the `*Messages*` buffer for warnings
3. Review package-specific logs (e.g., `*lsp-log*` for LSP issues)
