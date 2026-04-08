#!/bin/sh -e

set -e

cd ~

export TERM=xterm-256color  # make sure we have a widely used terminfo

echo_header() {
    echo
    echo " => $1 "
    echo
}

echo_header "Cloning dotfiles"

git clone --bare git@github.com:dccsillag/dotfiles.git ~/.dotfiles.git
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME checkout -f
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME push --set-upstream origin master
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.bare false
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.worktree $HOME
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config status.showUntrackedFiles no
. .profile

echo_header "Mise en place"

curl https://mise.run | sh
mise install

echo_header "Installing ZSH, if necessary"

if ! command -v zsh >/dev/null 2>&1
then
    # install zsh to ~/.local/bin/zsh:
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/romkatv/zsh-bin/master/install)" -- -d ~/.local -e no

    echo "NOTE: adding zsh call to ~/.bash_profile. Check that this works on this machine!"
    ( echo '$HOME/.local/bin/zsh'; echo 'exit $?' ) >> ~/.bash_profile
else
    echo "ZSH already installed, good to go!"
fi

echo_header "Adding TERM=xterm-256color to profile if kitty is not installed"

# If kitty isn't installed, let's be sure to set the TERM to xterm256-color in the local profile
if [ ! command -v kitty >/dev/null 2>&1 ]
then
    echo 'export TERM=xterm-256color' >> ~/.profile_local
do

echo
echo
echo 'Done! Now relog to finish the setup.'
echo '  Protip: keep this login open, and just log in in another session/ssh instance.'
