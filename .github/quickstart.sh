#!/bin/sh -ex

git clone --bare https://github.com/dccsillag/dotfiles.git ~/.dotfiles.git
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME checkout -f
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME push --set-upstream origin master
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.bare false
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.worktree $HOME
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config status.showUntrackedFiles no
. .profile
sh -c "$(curl https://raw.githubusercontent.com/dccsillag/packs/main/packs.sh)"

echo
echo
echo 'Ok! Now relog to finish the setup.'
echo '  Protip: keep this login open, and just log in in another session/ssh instance.'
