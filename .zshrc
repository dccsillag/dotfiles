#!/bin/zsh
#            _
#  ____ ___ | |__   _ __  ___
# |_  // __|| '_ \ | '__|/ __|
#  / / \__ \| | | || |  | (__
# /___||___/|_| |_||_|   \___|
#
# @author Daniel Csillag (aka. dccsillag)
# @what My ZSH configuration.

# zmodload zsh/zprof

source ~/.profile

source ~/.config/zgen/zgen.zsh

if ! zgen saved; then
    echo "Creating a zgen save"

    # zgen oh-my-zsh

    # plugins
    zgen load denysdovhan/spaceship-prompt spaceship
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-autosuggestions
    zgen load marzocchi/zsh-notify
    zgen load olets/zsh-abbr
    zgen load ael-code/zsh-colored-man-pages
    zgen load woefe/vi-mode.zsh

    # Abbreviations
    # # Remove all
    for abbrev in $(abbr list)
    do
        abbr -U erase $abbrev
    done
    # # Create
    # # # Edit Configs
    # abbr -U vimrc='nvim ~/.config/nvim/init.vim'
    abbr -U zshrc='nvim ~/.zshrc'
    abbr -U bibs='nvim ~/library/bibs.bib'
    # # # System management
    abbr -U defopen='mimeopen -d'
    abbr -U fre='free -h'
    # # # Git
    abbr -U gad='git add'
    abbr -U gib='git checkout'
    abbr -U gic='git commit'
    abbr -U gid='git diff'
    abbr -U gif='git fetch'
    abbr -U gil='git log'
    abbr -U gim='git merge'
    abbr -U gini='git init'
    abbr -U gip='git pull'
    abbr -U gipu='git push'
    abbr -U gir='git rebase'
    abbr -U gis='git status'
    abbr -U gisl='git shortlog'
    abbr -U gisu='git submodule'
    # # # Git Bare (dotfiles)
    abbr -U cad='config add'
    abbr -U cib='config checkout'
    abbr -U cic='config commit'
    abbr -U cid='config diff'
    abbr -U cif='config fetch'
    abbr -U cil='config log'
    abbr -U cim='config merge'
    abbr -U cip='config pull'
    abbr -U cipu='config push'
    abbr -U cir='config rebase'
    abbr -U cis='config status'
    abbr -U cisl='config shortlog'
    # # # NNN
    abbr -U n='nnn'
    # # # Haskell
    abbr -U hs='stack ghci'
    abbr -U st='stack'
    # # # Tree
    abbr -U tre='tree'

    zgen save
fi

# Spaceship theme configs:
SPACESHIP_PROMPT_ADD_NEWLINE=false
# SPACESHIP_PROMPT_ORDER=( user host dir git node exec_time line_sep jobs exit_code char )
SPACESHIP_PROMPT_ORDER=(
    user
    host
    dir
    git_branch
    jobs
    # vi_mode
    char
)
SPACESHIP_RPROMPT_ORDER=(
    exec_time
    exit_code
)
SPACESHIP_EXIT_CODE_SHOW=true
SPACESHIP_EXIT_CODE_SYMBOL='✗ '
SPACESHIP_CHAR_SYMBOL='$ '
# SPACESHIP_CHAR_SYMBOL='→ '
# SPACESHIP_CHAR_SYMBOL='⇒ '
# SPACESHIP_CHAR_SYMBOL='@ '

# Autosuggestions Color
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=239"

# Done Notifications
zstyle ':notify:*' command-complete-timeout 10
zstyle ':notify:*' app-name zsh
zstyle ':notify:*' error-title "Command FAILED (in #{time_elapsed})"
zstyle ':notify:*' success-title "Command finished (in #{time_elapsed})"

# History
# # How much to save
HISTSIZE=10000
SAVEHIST=10000
# # Save to file
mkdir -p ~/.local/misc/zsh/
HISTFILE=~/.local/misc/zsh/history
# # Save timestamps in history (FIXME):
HIST_STAMPS="mm/dd/yyyy"

# Reset xterm style after SSH
function ssh() {
    export SSHALIAS=1
    /usr/bin/ssh $@
    export SSHALIAS=0
    tmux select-pane -P 'bg=black'
}

# Open files with external applications
function open() {
    for file in $@
    do
        xdg-open $file & disown
    done
}

# Change directory with NNN
function ncd() {
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Open vim-fugitive
function vit() {
    if [ $PWD = / ]
    then
        echo "Not a git repository"
        return 1
    fi
    if [ -d .git/ ]
    then
        nvim .git/index
    else
        ( cd .. && vit )
    fi
}
alias cit='nvim ~/.dotfiles.git/index'

# Setup aliases
# # Vim
alias vim=nvim
alias edit=nvim
alias view='nvim -R'
# # Config
# alias config='git --git-dir=$HOME/.dotfiles.git --work-tree=$HOME'
# # cp / mv / rm
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
# # LS
#alias ls='ls -h --color=auto'
#alias ll='ls -lh --color=auto'
#alias la='ls -lAh --color=auto'
if which exa > /dev/null
then
    alias ls='exa -s type'
    alias ll='exa -l -s type'
    alias la='exa -la -s type'
    alias tree='exa --tree -s type'
else
    alias ls='ls -h --color=auto'
    alias ll='ls -hl --color=auto'
    alias la='ls -hAl --color=auto'
    alias tree='tree -C'
fi
# # Grep
alias grep='grep --color=auto'
# # Directories
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'
# # misc
alias clear='clear -x'
alias cls='/usr/bin/clear'
alias cmd='command'
# # Edit a file in Vim
alias ed='f=$(fzf) && vim $f'
alias ce='f=$(config ls-tree --full-tree -r --name-only HEAD | sed "s|^|$HOME/|" | fzf) && vim $f'
alias ge='f=$(git ls-tree --full-tree -r --name-only HEAD | fzf) && vim $f'

# Setup better history completion
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# Setup completion menu colors
zstyle ':completion:*' menu select

# Enable comments
setopt interactive_comments

# ---

function greet() {
    # Show a fun greeting!
    RANDCOLOR=$(echo -e "31\n32\n33\n34\n35\n36\n37" | shuf -n 1)
    echo -n "\e[2;3;${RANDCOLOR}m"
    # fortune -s -e calvin chucknorris bofh-excuses protolol question-answer-jokes brasil
    fortune -s 50% calvin 15% chucknorris 15% question-answer-jokes 15% protolol 5% bofh-excuses
    echo -n '\e[0m'
    unset RANDCOLOR
    echo
}

# which fortune > /dev/null && greet

if [ -z "$TMUX" ]
then
    tmux
    exit
fi

# zprof
