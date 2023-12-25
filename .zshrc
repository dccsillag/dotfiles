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

# Setup starship prompt
eval "$(starship init zsh)"

# Remove right prompt
unset RPS1

# Autosuggestions Color
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=239"

# Done Notifications
zstyle ':notify:*' command-complete-timeout 10
zstyle ':notify:*' app-name zsh
zstyle ':notify:*' error-title "Command FAILED (in #{time_elapsed})"
zstyle ':notify:*' success-title "Command finished (in #{time_elapsed})"

# Abbreviations temp directory
ABBR_TMPDIR="$HOME/.local/share/zsh-abbr/"

[ -f ~/.zgen/zgen.zsh ] || git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"

source ~/.zgen/zgen.zsh

if ! zgen saved; then
    echo "Creating a zgen save"

    # zgen oh-my-zsh

    # plugins
    zgen load Aloxaf/fzf-tab
    zgen load chisui/zsh-nix-shell
    zgen load zsh-users/zsh-syntax-highlighting
    # zgen load zsh-users/zsh-autosuggestions
    zgen load marzocchi/zsh-notify
    zgen load ael-code/zsh-colored-man-pages
    zgen load woefe/vi-mode.zsh
    zgen load pawel-slowik/zsh-term-title
    zgen load olets/zsh-abbr

    # Abbreviations
    # # Remove all
    for abbrev in $(abbr list | cut -d= -f1)
    do
        abbr -U erase $abbrev
    done
    # # Create
    # # # System management
    abbr -U defopen='mimeopen -d'
    abbr -U fre='free -h'
    # # # Git
    abbr -U gad='git add'
    abbr -U gib='git branch'
    abbr -U gich='git checkout'
    abbr -U gic='git commit'
    abbr -U gid='git diff'
    abbr -U gisd='GIT_EXTERNAL_DIFF="difft --display=inline" git diff'
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
    abbr -U gilr='git log @{1}..'
    # # # Git Bare (dotfiles)
    abbr -U cad='config add'
    abbr -U cib='config branch'
    abbr -U cicr='config checkout'
    abbr -U cic='config commit'
    abbr -U cid='config diff'
    abbr -U cisd='GIT_EXTERNAL_DIFF="difft --display=inline" config diff'
    abbr -U cif='config fetch'
    abbr -U cil='config log'
    abbr -U cim='config merge'
    abbr -U cip='config pull'
    abbr -U cipu='config push'
    abbr -U cir='config rebase'
    abbr -U cis='config status'
    abbr -U cisl='config shortlog'
    abbr -U cist='config stash'
    abbr -U cilr='config log @{1}..'
    # # # MicroMusic
    abbr -U mca='mcm append'
    abbr -U mcp='mcm prepend'
    abbr -U mcl='mcm list'
    abbr -U mcs='mcm status'
    abbr -U mcr='mcm restart'
    abbr -U mce='mcm edit'
    # # # NNN
    abbr -U n='nnn'
    # # # Haskell
    abbr -U hs='stack ghci'
    abbr -U --force st='stack'
    # # # Tree
    abbr -U tre='tree'
    # # # Abduco
    abbr -U ab='abduco'
    # # # Taskell
    abbr -U ta='taskell'
    # # # BluetoothCTL
    abbr -U bu='bluetoothctl'
    # # # Vim
    abbr -U --force v='nvim'
    # # # Nix
    abbr -U ns='nix-shell'
    abbr -U nsp='nix-shell -p'
    #abbr -U nps='nix-shell --pure'
    #abbr -U npsp='nix-shell --pure -p'
    # # # Rum
    abbr -U --force r='rum'
    # # # Cobench
    abbr -U --force co='cobench'

    zgen save
fi

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
    /usr/bin/env ssh "$@"
    export SSHALIAS=0
    exitcode=$?
    bgset %0
    return $exitcode
}
function mosh() {
    export SSHALIAS=1
    /usr/bin/env mosh "$@"
    export SSHALIAS=0
    exitcode=$?
    bgset %0
    return $exitcode
}

# fg, bg
function fgbg() {
    command="$1"

    if [ "$#" -eq 1 ]; then
        builtin "$command"
    elif [ "$#" -eq 2 ]; then
        builtin "$command" %"$2"
    else
        echo "bad job number: $2" 1>&2
    fi
}
alias fg="fgbg fg"
alias bg="fgbg bg"
alias disown="fgbg disown"

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

# Goto tag
function te() {
    [ "$PWD" = "/" ] && return
    [ -f tags ] || {
        ( cd .. && te $@ )
        return
    }

    t=$(grep -v '^!_' tags | fzf-inline --nth 1..2 -d '\t' --tiebreak=begin)
    f=$(echo "$t" | cut -f 2)
    p=$(echo "$t" | cut -f 3 | sed 's|^/\^\(.\+\)\$/;"$|\1|')
    l=$(grep -F -n -m 1 "$p" "$f" | sed 's/^\([0-9]\+\):.\+$/\1/')
    [ -n "$f" ] && [ -n "$l" ] && nvim "$f" +"$l"
}

# Open vim-fugitive
alias vit='nvim +Fugitive'
alias cit='( cd ~ && GIT_DIR="$HOME/.dotfiles.git" GIT_WORK_TREE="$HOME" vit )'

# Open gv
alias giv='nvim +GV'

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

# Create and cd into a directory
function take() {
    [ "$#" -ne 1 ] && { echo 'Usage: take <DIR_NAME>' 2>&1; return 2; }
    mkdir -p "$1" && cd "$1"
}

# Manage the calendar
# function cal() {
#     test -z "$1" && NMONTHS=2 || { NMONTHS=$1; shift 1; }
#     remind -b1 -@2 -w$(tput cols) -cu$NMONTHS $@ ~/agenda/calendar/ | sed 's/\o14/\n\n\n\n\n\n\n\n/g' | less -r
# }
# function rem() {
#     remind -b1 -dl -@2 -g $@ ~/agenda/calendar/
# }

# Setup aliases
# # Vim
alias edit=nvim
alias view='nvim -R'
# # Sudo
( which doas > /dev/null 2>&1 ) && alias sudo='doas -- '
# # Python
alias python='python3'
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
    alias tree='exa --tree -s type --git-ignore'
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
alias cls='clear -x'
alias cmd='command'
# # Bottom
alias btmb='btm -ba'
alias btmp="btm -C $HOME/.config/bottom/bottom-proc.toml"
# # Vim
alias vi=nvim
alias vim=nvim
# # Edit a file in Vim
alias fzf-inline='fzf --height 50% --reverse'
alias ed='f=$(fzf-inline) && gvim $f'
alias ce='f=$(config ls-tree --full-tree -r --name-only HEAD | sed "s|^|$HOME/|" | fzf-inline) && ( cd ~ && GIT_DIR="$HOME/.dotfiles.git" GIT_WORK_TREE="$HOME" gvim $f )'
alias ge='f=$({ cd $(git rev-parse --show-toplevel); realpath $(git ls-tree --full-tree -r --name-only HEAD | fzf-inline) }) && gvim $f'
alias age='f=$(ls ~/agenda/calendar/*.rem | fzf-inline) && gvim $f'

# Setup better history completion
if [ -z "$key" ]; then
    bindkey "^[[A" history-beginning-search-backward
    bindkey "^[[B" history-beginning-search-forward
else
    bindkey "$key[Up]" history-beginning-search-backward
    bindkey "$key[Down]" history-beginning-search-forward
fi

# Enable comments
setopt interactive_comments

test -f ~/.zshrc_local && source ~/.zshrc_local

show_message() {
    echo "$(tput bold)$1$(tput sgr0)"
}

# Auto nix-shell
auto_nix_shell() {
    if [ -n "$IN_NIX_SHELL" ]
    then
        # Already in a nix-shell
        if ! [ -f default.nix ]
        then
            if [[ $PWD != "$AUTO_NIX_SHELL_DIR"* ]]
            then
                show_message "exiting nix-shell"
                echo "$PWD" > "$AUTO_NIX_PWD_FILE"
                exit
            fi
        fi
    else
        # Not in a nix-shell
        if [ -f default.nix ] && command -v nix-shell > /dev/null
        then
            show_message "detected default.nix; entering nix-shell..."
            export AUTO_NIX_SHELL_DIR="$PWD"
            export AUTO_NIX_PWD_FILE="$(mktemp /tmp/zsh-autonix.XXXXXX)"
            nix-shell
            exit_code="$?"
            if [ -s "$AUTO_NIX_PWD_FILE" ]
            then
                cd "$(cat "$AUTO_NIX_PWD_FILE")"
                command rm "$AUTO_NIX_PWD_FILE"
                unset AUTO_NIX_SHELL_DIR
                unset AUTO_NIX_PWD_FILE
            else
                command rm "$AUTO_NIX_PWD_FILE"
                exit "$exit_code"
            fi
        fi
    fi
}

# Julia auto-project
# julia() {
#     [ -z "$ORIGINAL_CWD" ] && export ORIGINAL_CWD="$PWD"
#
#     if [ "$PWD" = / ]; then
#         cd "$ORIGINAL_CWD"
#         command julia "$@"
#     elif [ -f Project.toml ]; then
#         echo "Running julia with --project=. from $PWD"
#         command julia --project=. "$@"
#     else
#         (
#             cd ..
#             julia "$@"
#         )
#     fi
# }

#chpwd_functions+=(auto_nix_shell)

{ which zoxide >/dev/null 2>&1 } && eval "$(zoxide init zsh)"

if { which direnv &> /dev/null; }
then
    eval "$(direnv hook zsh)"
fi

unset '_comps[nail]'

# ---

# function greet() {
#     if [ -d .git ]
#     then
#         command -v onefetch >/dev/null && onefetch
#     else
#         command -v pfetch >/dev/null && pfetch
#     fi
# }
#
# greet
