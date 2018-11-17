# My emacs configuration

Clone the repository into `~/.emacs.d/_0x17de/` and install the submodules:
```
git clone --recursive https://github.com/0x17de/emacs-config/ ~/.emacs.d/_0x17de/
```

After that the following should be your .emacs file:

```
(add-to-list 'load-path "~/.emacs.d/_0x17de/")
(load "_0x17de-emacs")
```

Dependencies will be installed automatically on the first run.

Things to run once the setup is finished:
```
M-x irony-install-server
```
