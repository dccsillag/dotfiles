#!/bin/sh

git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.bare false
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.worktree $HOME
