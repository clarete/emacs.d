# Emacs setup

This repository contains my Emacs setup, these configurations are is
pretty personal but you might find a few of them useful and improve
the configuration of your Emacs.

## Install

This is how you can get your Emacs to use this repository as the base
for your configuration.

```bash
 $ git clone http://github.com/clarete/emacs.d
 $ ln -s $(pwd)/emacs.d ~/.emacs.d
 $ cd ~/.emacs.d && make
```

The above command will install [Cask](https://github.com/cask/cask) and
all the emacs extensions described in the `Cask` file.