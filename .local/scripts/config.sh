#!/bin/sh

args=$({ for arg in $@; do echo $arg; done; })

git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME $(echo "$args" | grep -v [-]-git-dir | grep -v [-]-work-tree)
