#!/bin/sh

# My environment variables.

# XDG vars

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share:$XDG_DATA_DIRS"
export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"

# Essential vars

# export LD_LIBRARY_PATH=/usr/local/lib

# Misc

export EDITOR="nvim"
export BROWSER="qutebrowser-quick"
export PATH="$PATH:$HOME/.local/share/cargo/bin:$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:.gem/ruby/2.7.0/bin:$HOME/.local/share/gem/bin"
export GOPATH="$XDG_DATA_HOME/go"
export SSHALIAS="0"
export OCTAVE_HISTFILE="$XDG_CACHE_HOME/octave-hsts"
export OCTAVE_SITE_INITFILE="$XDG_CONFIG_HOME/octave/octaverc"
export LESSKEY="$XDG_CONFIG_HOME/less/lesskey"
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"
export PLTUSERHOME="$XDG_DATA_HOME/racket"
export IPYTHONDIR="$XDG_CONFIG_HOME/jupyter"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export CONAN_USER_HOME="$XDG_DATA_HOME/conan"
export GEM_HOME="$XDG_DATA_HOME/gem"
export GEM_SPEC_CACHE="$XDG_CACHE_HOME/gem"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export PYLINTHOME="$XDG_CACHE_HOME/pylint"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export STACK_ROOT="$XDG_DATA_HOME/stack"
export NLTK_DATA="$XDG_DATA_HOME/nltk"
export NNN_SSHFS="sshfs -o reconnect,follow_symlinks"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/
export OPENER='open'
export LESS='SRX'
export PYTHONPATH=.

# NNN

export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='p:preview-tabbed'

test -f ~/.profile_local && . ~/.profile_local
