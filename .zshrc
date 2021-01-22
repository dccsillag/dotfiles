#!/bin/zsh
#            _
#  ____ ___ | |__   _ __  ___
# |_  // __|| '_ \ | '__|/ __|
#  / / \__ \| | | || |  | (__
# /___||___/|_| |_||_|   \___|
#
# @author Daniel Csillag (aka. dccsillag)
# @what My ZSH configuration.

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
    zgen load pawel-slowik/zsh-term-title

    # Abbreviations
    # # Remove all
    for abbrev in $(abbr list)
    do
        abbr -U erase $abbrev
    done
    # # Create
    # # # Edit Configs
    abbr -U vimrc='nvim ~/.config/nvim/init.vim'
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
    abbr -U gist='git stash'
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
    abbr -U cist='config stash'
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
    dir
    # conda
    git_branch
    jobs
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
SPACESHIP_JOBS_SYMBOL=''
SPACESHIP_JOBS_AMOUNT_THRESHOLD=0
SPACESHIP_JOBS_AMOUNT_PREFIX='['
SPACESHIP_JOBS_AMOUNT_SUFFIX=']'
SPACESHIP_GIT_BRANCH_SUFFIX=' '

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
    exitcode=$?
    bgset %0
    return $exitcode
}

# Open files with external applications
function open() {
    (($# < 1)) && echo 'bad params for open' && return 1
    for file in "$@"
    do
        nohup xdg-open "$1" &> /dev/null &!
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

# Extract a file
function extract() {
    if [ -f $1 ]
    then
        case $1 in
            *.tar.bz2)      tar xjf $1    ;;
            *.tar.gz)       tar xzf $1    ;;
            *.bz2)          bunzip2 $1    ;;
            *.rar)          unrar x $1    ;;
            *.gz)           gunzip $1     ;;
            *.tar)          tar xf $1     ;;
            *.tbz2)         tar xjf $1    ;;
            *.tgz)          tar xzf $1    ;;
            *.zip)          unzip $1      ;;
            *.Z)            uncompress $1 ;;
            *.7z)           7z x $1       ;;
            *.deb)          ar x $1       ;;
            *.tar.xz)       tar xf $1     ;;
            *.tar.zst)      unzstd $1     ;;
            *)              echo "'$1' cannot be extracted via ex()"
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Setup aliases
# # Vim
alias vim=nvim
alias edit=nvim
alias view='nvim -R'
# # Sudo
alias sudo='doas -- '
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
alias ge='f=$({ cd $(git rev-parse --show-toplevel); realpath $(git ls-tree --full-tree -r --name-only HEAD | fzf) }) && vim $f'

# Setup better history completion
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# Setup completion menu colors
zstyle ':completion:*' menu select

# Enable comments
setopt interactive_comments

test -f ~/.zshrc_local && source ~/.zshrc_local

# ---

function greet() {
    # Show a fun greeting!
    echo -n "\e[1;30m"
    fortune -s 5% calvin 5% chucknorris 5% question-answer-jokes 5% protolol 40% bofh-excuses 40% brasil | grep -v '^$'
    echo -n '\e[0m'
    echo
}

which fortune > /dev/null && greet

# if [ -z "$TMUX" ]
# then
#     tmux
#     exit
# fi
