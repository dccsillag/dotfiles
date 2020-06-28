# ▄▄                      ▄▄      ▄▄▄▄▄▄     ▄▄▄▄
# ██                      ██      ██▀▀▀▀██ ██▀▀▀▀█
# ██▄███▄  ▄█████▄▄▄█████▄██▄████▄██    ████▀
# ██▀  ▀██ ▀ ▄▄▄████▄▄▄▄ ▀██▀   █████████ ██
# ██    ██▄██▀▀▀██ ▀▀▀▀██▄██    ████  ▀██▄██▄
# ███▄▄██▀██▄▄▄████▄▄▄▄▄████    ████    ██ ██▄▄▄▄█
# ▀▀ ▀▀▀   ▀▀▀▀ ▀▀ ▀▀▀▀▀▀ ▀▀    ▀▀▀▀    ▀▀▀  ▀▀▀▀
#
# @what My .bashrc.
# @author Daniel Csillag (aka. dccsillag)
#
# @deprecated I've switched to FISH; I mean, just look at the first lines of this bashrc.

# fish
# exit

# vim: fdc=2
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto -N --g'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --hint=int:transient:1 --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export EDITOR=vim

#alias python3="python3.8"
#alias python="python3.8"
alias python="python3"
alias lua="lua5.3"

alias vix="vim -X"

alias open="~/Fixed/xdg_open_script.sh"

alias shout="alert; echo -e \"\\a\""

alias nasmc="~/Fixed/nasm_compile.sh"

export LUA_PATH_5_3=~/Fixed/lib/lua/?.lua
export LUVIT_PATH_5_3=~/Fixed/lib/lua/?.lua

# Setup jump/mark, as in https://www.datascienceworkshops.com/blog/quickly-navigate-your-filesystem-from-the-command-line/
export MARKPATH=$HOME/.marks
function jump {
    if [[ -z $1 ]]; then
        echo "Please specify a mark to jump to"
    else
        echo "Jump to mark $1"
        cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
    fi
}
function mark {
    echo "Create mark $1"
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    echo "Remove mark $1"
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
_completemarks() {
  local curw=${COMP_WORDS[COMP_CWORD]}
  local wordlist=$(find $MARKPATH -type l -printf "%f\n")
  COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
  return 0
}
complete -F _completemarks jump unmark

# Save execution timestamps in history
HISTTIMEFORMAT="(%F %T) "
# Save repeated commands (necessary for correct execution time measurement)
export HISTCONTROL=ignorespace
# Get time of the start of the Bash session
SESSIONSTARTTIME=$(date '+%s')

# Pretty print time
function displaytime {
    # taken from https://unix.stackexchange.com/a/27014/111254
    local T=$1
    local D=$((T/60/60/24))
    local H=$((T/60/60%24))
    local M=$((T/60%60))
    local S=$((T%60))
    (( $D > 0 )) && printf '%d days ' $D
    (( $H > 0 )) && printf '%d hours ' $H
    (( $M > 0 )) && printf '%d minutes ' $M
    (( $D > 0 || $H > 0 || $M > 0 )) && printf 'and '
    printf '%d seconds\n' $S
}

# Custom prompt:
PROMPT_COMMAND=__prompt_command
__prompt_command() {
    local EXIT="$?"             # This needs to be first
    PS1=""

    local RCol='\[\e[0m\]'      # Reset

    local Red='\[\e[0;31m\]'
    local Green='\[\e[0;32m\]'
    local Yelllow='\[\e[0;33m\]'
    local Blue='\[\e[0;34m\]'
    local Magenta='\[\e[0;35m\]'
    local Cyan='\[\e[0;36m\]'
    local Gray='\[\e[0;37m\]'
    local BrightRed='\[\e[1;31m\]'
    local BrightGreen='\[\e[1;32m\]'
    local BrightYellow='\[\e[1;33m\]'
    local BrightBlue='\[\e[1;34m\]'
    local BrightMagenta='\[\e[1;35m\]'
    local BrightCyan='\[\e[1;36m\]'
    local BrightGray='\[\e[1;37m\]'

    if [ $EXIT == 0 ]; then     # OK
        PS1+="${Green}\u${RCol}"
    elif [ $EXIT == 148 ]; then # Interrupt (Ctrl-z)
        PS1+="${Green}\u${RCol}"
    elif [ $EXIT == 130 ]; then # Cancel (Ctrl-C)
        PS1+="${RCol}\u${RCol}"
    elif [ $EXIT == 139 ]; then # Segfault
        PS1+="${BrightYelllow}\u${RCol}"
    else                        # General error
        PS1+="${BrightRed}\u${RCol}"
    fi

    local hasjobs=$(jobs -p)

    PS1+="${RCol}@${BrightGreen}\h${RCol}:${BrightBlue}\w${RCol}${hasjobs:+:}${BrightCyan}${hasjobs:+\j}${RCol}"

    ENDTIME=$(date '+%s')
    LAST_COMM="$(HISTTIMEFORMAT='%s '; history 1)"
    STARTTIME=$(awk '{print $2}' <<<"$LAST_COMM")
    # START_PRETTYTIME=$(date -d"@$START" '+%T %D')

    if [ $((ENDTIME - STARTTIME)) -gt 1 ] && [ $((ENDTIME - SESSIONSTARTTIME)) -gt 1 ]
    then
        local RCol='\e[0m'
        local Red='\e[0;31m'
        local Green='\e[0;32m'
        local Yelllow='\e[0;33m'
        local Blue='\e[0;34m'
        local Magenta='\e[0;35m'
        local Cyan='\e[0;36m'
        local Gray='\e[0;37m'
        local BrightRed='\e[1;31m'
        local BrightGreen='\e[1;32m'
        local BrightYellow='\e[1;33m'
        local BrightBlue='\e[1;34m'
        local BrightMagenta='\e[1;35m'
        local BrightCyan='\e[1;36m'
        local BrightGray='\e[1;37m'

        if [ $EXIT == 0 ]
        then
            echo -e "${BrightGreen} ✓ ${Green}took ${BrightMagenta}$(displaytime $((ENDTIME - STARTTIME)))${RCol}"
        elif [ $EXIT == 148 ]
        then
            :
        elif [ $EXIT == 130 ]
        then
            echo -e "${BrightCyan} ¬ ${Cyan}canceled; took ${BrightMagenta}$(displaytime $((ENDTIME - STARTTIME)))${RCol}"
        else
            echo -e "${BrightRed} ✗ ${BrightRed}took ${BrightMagenta}$(displaytime $((ENDTIME - STARTTIME))) ${BrightRed}(exited with error code ${BrightBlue}${EXIT}${BrightRed})${RCol}"
        fi
    fi

    PS1+="\$ "

    #PS1+="${BrightGreen}\u@\h${RCol}:${BrightBlue}\W${RCol}"
    #PS1+="${RCol}\$ "
}


# For LineageOS install
#export USE_CCACHE=1
#export ANDROID_JACK_VM_ARGS="-Dfile.encoding=UTF-8 -XX:+TieredCompilation -Xmx4G"

# Protect myself from [being stupid and] overwriting things with cp and mv.
alias mv="mv -i"
alias cp="cp -i"

# Setup googler
export BROWSER=w3m googler query
alias g="googler --noua -c com -l english -x --colors bjdxxy"

# Setup fuck
if [ -x "$(command -v thefuck)" ]; then
    eval $(thefuck --alias)
fi

# Keep OLDPWD between shell sessions (for `cd -`)
# trap 'pwd > ~/.lwd' EXIT
#
# test -f ~/.lwd && export OLDPWD=`head -1 ~/.lwd`

# An alias for GHCi (stack)
alias hs="stack ghci"

if [ -x "$(command -v stack)" ]; then
    # Add [Haskell] stack autocompletion
    eval "$(stack --bash-completion-script stack)"
fi

if [ -x "$(command -v register-python-argcomplete)" ]; then
    # Setup ToM
    eval "$(register-python-argcomplete tom)"
    # eval "$(register-python-argcomplete ~/Code/tom-core/main.py)"

    # Setup DevTask (tsk)
    eval "$(register-python-argcomplete tsk)"
fi

if [ -e ~/Fixed/flutter_completion.sh ]; then
    # Setup flutter
    . ~/Fixed/flutter_completion.sh
fi

# Setup better-exceptions for Python
export BETTER_EXCEPTIONS=1

# Hack Make into using colors for errors and warnings
make() {
    pathpat="(/[^/]*)+:[0-9]+"
    ccred=$(echo -e "\033[0;31m")
    ccyellow=$(echo -e "\033[0;33m")
    ccend=$(echo -e "\033[0m")
    /usr/bin/make "$@" 2>&1 | sed -E -e "/[Ee]rror[: ]/ s%$pathpat%$ccred&$ccend%g" -e "/[Ww]arning[: ]/ s%$pathpat%$ccyellow&$ccend%g"
    return ${PIPESTATUS[0]}
}

# Configure Pandoc to use some filters I like.
alias pandoc="pandoc --filter pandoc-crossref"
# alias pandoc="pandoc --filter pandoc-theorem-exe --filter pandoc-crossref"

# Configure SSH to change colors
function ssh_alias() {
    export SSHALIAS=1;
    ssh $@;
    export SSHALIAS=0;
    xtermcontrol;
}
alias ssh=ssh_alias

alias gitvim="vim .git/index"

# Load local configs
if [ -e $HOME/.bashrc_local ]; then
    source $HOME/.bashrc_local
fi

alias tre=tree

function vit() {
    if [ `pwd` = / ]
    then
        echo "Not a git repository" 1>&2
        return 1
    elif [ -d .git/ ]
    then
        vim .git/index
    else
        ( cd .. && vit )
    fi
}

alias gis="git status"
alias gad="git add"
