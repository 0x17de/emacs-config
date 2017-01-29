# My emacs configuration

Clone the repository into `~/.emacs.d/_0x17de/` and install the submodules:
```
git clone https://github.com/0x17de/emacs-config/ ~/.emacs.d/_0x17de/
cd ~/.emacs.d/_0x17de/
git submodule init
git submodule update
```

After that the following should be your .emacs file:

```
(add-to-list 'load-path "~/.emacs.d/_0x17de/")
(load "_0x17-emacs")
```
