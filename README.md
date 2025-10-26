# Memacs
Minimalistic programming focused emacs distribution with quick start up time.

- Fast startup time
- Support for PHP, C, C++, JS, Typescript, Go
- Line numbers
- Language Server integration
- Dashboard with recent files and bookmarks
- Emacs Server
- Auto complete with company
- Jump to definition with xref M-.
- C-c C-c to execute php code blocks in markdown mode
- Add a comment `#x <command>` in any source file and run it with point on the line and C-c C-c
## Install
Install in a fresh emacs with
```
git clone https://github.com/hhirsch/memacs.git $HOME/.emacs.d
```

## Run As Service
Run emacs as service
```
systemctl --user enable --now emacs
```

Check if the service is running
```
systemctl --user status emacs.service
```

To start emacs type
```
emacsclient
```

For extra performance I suggest starting emacsclient in the terminal it will look and feel the same:
```
emacsclient -nw
```
