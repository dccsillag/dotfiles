#!/bin/sh -ex

cd ~

export TERM=xterm-256color  # make sure we have a widely used terminfo

git clone --bare git@github.com:dccsillag/dotfiles.git ~/.dotfiles.git
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME checkout -f
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME push --set-upstream origin master
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.bare false
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config core.worktree $HOME
git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME config status.showUntrackedFiles no
. .profile

curl https://mise.run | sh
mise install

if ! command -v zsh >/dev/null 2>&1
then
    # install zsh:
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/romkatv/zsh-bin/master/install)"

    if [ ! -f "$HOME/.local/bin/zsh" ]
    then
        ( echo '$HOME/.local/bin/zsh'; echo 'exit $?' ) >> ~/.bash_profile  # TODO make this more seamless
    do
fi

echo
echo
echo 'Ok! Now relog to finish the setup.'
echo '  Protip: keep this login open, and just log in in another session/ssh instance.'
