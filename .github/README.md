dotfiles
========

This a Bare repository with my dotfiles.

The following Linux distributions are supported:

- Ubuntu
- Manjaro (this likely implies Arch as well)

Installation
------------

**WARNING:** This is NOT:

- Tested
- Working

First, clone this repository (keep in mind this is a Git Bare repository) and
set its proper git configs:

```sh
cd
git clone --bare git@github.com:dccsillag/dotfiles.git ~/.dotfiles.git
sh .csillag/post-clone.sh
```

Next, run the install script for packages (this will take a while):

```sh
cd ~/.csillag
./install-packages.sh
```
