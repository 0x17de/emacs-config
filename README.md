# My Emacs Configuration

## New to Emacs?

If you're new to Emacs, check out these resources to get started:
- [Essential Emacs Hotkeys](https://blog.0x17.de/post/essential-emacs-hotkeys/)
- [Emacs Lisp Introduction](https://blog.0x17.de/post/emacs-list-introduction/)
- [Building a Modular Emacs Configuration](https://blog.0x17.de/post/modular-emacs-config/)

## Contributing

I welcome feature requests and bug reports. Feel free to open an issue on GitHub or contact me directly.

## Overview

A modular Emacs configuration with comprehensive language support, development tools, and UI enhancements. Designed for easy extension and maintenance, with particular focus on developer experience.

## Features

### General
- Easy installation of the configuration (packages are automatically installed via use-package)
- Performance optimizations for faster startup and responsiveness
- Frame and window management utilities (resize, zoom, tiling, etc.)
- Multiple cursor editing
- Snippets (yasnippet; user-defined ones can be put into ~/.emacs.d/snippets)
- In-buffer completion via corfu and cape, fed by LSP and the major modes
- Documentation lookup integration
- Many quality-of-life improvements for editing
- Embark integration for contextual actions

### Development Tools
- Version control using Magit
- Language Server Protocol (LSP) support
- Syntax checking via Flycheck
- Format on save via apheleia (ruff for Python, goimports for Go)
- Comprehensive support for project management via Projectile
- Terminal integration via VTerm

### Language Support

Language support is primarily based on LSP and/or specialized modes:

- **C/C++**: LSP (clangd) + corfu + flycheck + CMake integration
- **Python**: basedpyright + corfu + flycheck, ruff on save
- **Go**: LSP (gopls), goimports on save
- **Rust**: rust-analyzer + cargo integration

Python, Go and Rust use the tree-sitter major modes when their grammars are installed, and the classic modes otherwise.
- **LaTeX**: AUCTeX with compile-on-save functionality
- **Web**: HTML, CSS, JavaScript support
- **PlantUML**: Integrated preview and automatic generation of diagrams
- **Org-mode**: Enhanced with org-modern, org-journal, and custom babel support
- **Ansible**: YAML mode with specialized snippets and formatting
- **Misc**: Support for YAML, JSON, Docker, and many others

### Additional Features
- Confluence export from Org mode documents
- Google Styleguide integration

## Setup

Clone the repository to anywhere and install the submodules:
```
git clone --recursive https://github.com/0x17de/emacs-config/ /path/to/_0x17de-emacs-config/
```

Here's how your `.emacs` file could be structured. The most critical element is the load command. If you're unsure about the location of your `.emacs` file, simply execute `M-: user-init-file RET` (press Alt and colon together, type user-init-file, then hit Enter) in Emacs, and the minibuffer will display the path to your initialization file.

```elisp
(package-initialize)

;; optional org mode encryption settings
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "123...GPGID...DEF")

;; optional for rust configuration (if you need Rust)
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "HOME") "/.cargo/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))

;; INFO: Your custom config can be either here or inside /path/to/_0x17de-emacs-config/custom.el


;; IMPORTANT: This will actually enable this configuration
(load "/path/to/_0x17de-emacs/_0x17de-emacs")

;; optional for hugo blogging
;; (setq easy-hugo-basedir "~/hugo/0x17blog")
```

Also load `early-init.el` from `~/.emacs.d/early-init.el`. It holds the startup GC settings, and hides the bars before the first frame is drawn:

```elisp
(load "/path/to/_0x17de-emacs/early-init" nil t)
```

## First-Time Setup

LSP and language features will need additional setup:

### For Go development
Run once to install Go language tools:
```
M-x ox-install-go-dependencies
```

### Tree-sitter grammars (optional)
Python, Go and Rust switch to their tree-sitter modes when the grammars are available, e.g. in `~/.emacs.d/tree-sitter` (`M-x treesit-install-language-grammar`, or from Nix: `emacs.pkgs.treesit-grammars.with-grammars`).

### emacs-lsp-booster (optional)
With [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) on `PATH`, language servers run behind it and lsp-mode spends less time parsing JSON. lsp-mode is configured for plists (`LSP_USE_PLISTS`); after enabling that for the first time, reinstall or recompile lsp-mode and the packages depending on it.

### For C/C++ development
Install `clangd` (part of `clang-tools` on NixOS/Debian, `clang` on Arch). Without it on `PATH`, C/C++ buffers still get completion, flycheck and the Google style, but no LSP features.

### For Java development
Install the Java language server:
```
M-x lsp-install-server RET jdtls RET
```

### For PlantUML
Set the path to your PlantUML jar file via `M-x customize-group RET _0x17de RET`, then make `_0x17de/plantuml-jar-file` point to the jar file location.

## Directory Structure

The configuration is organized into several directories:

- `early-init.el`: Startup GC and frame settings, loaded from `~/.emacs.d/early-init.el`
- `init/`: Core initialization files
  - `custom.el`: Loads `custom.el` from the repository root as `custom-file`
  - `package.el`: Package management
  - `speedup.el`: Performance optimizations
- `langs/`: Language-specific configurations
- `utils/`: Utility functions and tools
- `snippets/`: YASnippet templates
- `ext/`: External packages and resources

## Customization

You can put your own customizations in either your `.emacs` file or in `custom.el` at the root of this repository (git-ignored), which is used as `custom-file` and loaded automatically if it exists.

## Hotkeys

| Hotkey          | Description                                                          |
|-----------------|----------------------------------------------------------------------|
| `C-M-z \|`      | Multiple cursors: select all instances of the selected region        |
| `C-M-z C-c`     | Multiple cursors: place cursor in every line of selected region      |
| `C-M-z C-M-c`   | Multiple cursors: place cursor on non-empty lines of selected region |
| `C-M-z >`       | Multiple cursors: add a cursor at the next matching region           |
| `C-M-z <`       | Multiple cursors: add a cursor at the previous matching region       |
| `M-<mouse1>`    | Multiple cursors: add/remove a cursor at the pointer location        |
| `C-M-S-y`       | Show snippets for current mode                                       |
| `C-M-S-q`       | Close current buffer without asking                                  |
| `C-M-S-w`       | Close current buffer and window without asking                       |
| `C-M-\|`        | Pipe region through shell command and replace with result            |
| `S-<tab>`       | Insert a real tab character instead of spaces                        |
| `<f1>`          | Show documentation for item at point (LSP; candidate docs in popup)  |
| `<f12>`         | LSP: go to definition                                                |
| `S-<f12>`       | LSP: find references                                                 |
| `C-S-j`         | Go to definition (xref; dumb-jump where no LSP is running)           |
| `<f5>`          | Generic compile command (project-specific actions in some modes)     |
| `<tab>`         | Indent, then complete; expands a snippet key before point            |
| `C-<tab>`       | Complete at point                                                    |
| `C-x u`         | Visual undo tree (vundo)                                             |
| `C-x o`         | Smart navigation between windows/frames                              |
| `M-<up/down>`   | Move the current line or region up/down                              |
| `C-M-S-<arrows>`| Move the current buffer into the window in that direction            |
| `C-M-<up/down>` | Increase/decrease the default font size                              |
| `C-:`           | [Avy](https://github.com/abo-abo/avy)-goto-char                      |
| `C-M-:`         | [Avy](https://github.com/abo-abo/avy)-goto-char-timer                |

## Language-Specific Features

### C/C++ Projects
Handled by `clangd` through lsp-mode. clangd resolves includes from a `compile_commands.json` in the project root, which CMake generates with `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` (symlink or copy it out of the build directory). Place a `.dir-locals.el` in the project root for project-specific configuration.

### Python Projects
Requires `basedpyright` to be installed (recommended via `pipx install basedpyright`), plus `ruff`, which sorts imports and formats on save.

### Go Projects
Automatically formats code with goimports on save.

### LaTeX Projects
Use the minor mode `latex-compile-on-save-mode` to automatically compile on saving.

### PlantUML Projects
Automatically generates and previews diagrams on save for `.puml` files.

## Customization Options

The configuration uses a custom group called `_0x17de` with many customizable variables. You can use `M-x customize-group RET _0x17de RET` to see all options. Here are the main customizable features:

### General Options
- **_0x17de/backup-file-location**: Directory for backup files (default: "~/.emacs.d/backups")
- **_0x17de/focus-follows-mouse**: Enable focus follows mouse (default: t)
- **_0x17de/scroll-amount**: Number of lines to scroll at a time (default: 5)
- **_0x17de/modeline**: Modeline style to use (default: doom-modeline)
- **_0x17de/minibuffer-completion-framework**: Completion framework (default: vertico)

### Language-Specific Options
- **_0x17de/plantuml:enable**: Enable PlantUML mode (default: t)
- **_0x17de/plantuml-jar-file**: Path to PlantUML jar file
- **_0x17de/lisp-paredit:enabled**: Enable paredit for Lisp modes (default: nil)
- **_0x17de/python-lsp-ruff-enable**: Run the ruff LSP server alongside basedpyright for live linting (default: nil)
- **_0x17de/nix-mode-enable**: Enable nix-mode with nixd LSP support (default: nil)
- **_0x17de/google-c-style-overrides**: Customize C/C++ formatting
- **_0x17de/mmm-mode:enable**: Enable mmm-mode (default: nil)
- **_0x17de/mmm-mode:babel:enable**: Enable org-babel support for mmm-mode (default: nil)

### Environment Options
- **_0x17de/exec-path-from-shell:enable**: Import environment from shell (default: nil)

You can customize these options either through the customize interface or by setting them in your `.emacs` file.

## Troubleshooting

If you encounter issues with the configuration:
1. Start Emacs with `emacs --debug-init` to see initialization errors
2. Check the `*Messages*` buffer for warnings
3. Review package-specific logs (e.g., `*lsp-log*` for LSP issues)
