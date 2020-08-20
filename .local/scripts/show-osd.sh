#!/bin/sh

set -e

# Default settings
if [ $# -gt 1 ]
then
    WIDTH=350
    HEIGHT=200
else
    WIDTH=250
    HEIGHT=250
fi
FONT="FantasqueSansMono Nerd Font-120"
TIMEOUT=1

# Load external configs
[ -f ~/.config/show-osd/config.sh ] && . ~/.config/show-osd/config.sh
[ -f ~/.show-osd.rc ] && . ~/.show-osd.rc

# Get middle of screen
NSCREENS=$(xrandr | grep -c "\<connected\>")
MIDX=$(($(xwininfo -root | grep Width: | sed 's/^  Width: \([0-9]\+\)$/\1/') / NSCREENS / 2))
MIDY=$(($(xwininfo -root | grep Height: | sed 's/^  Height: \([0-9]\+\)$/\1/') / 2))

# Create the window
( for arg in "$@"; do echo "$arg"; done ) | dzen2 -p \
    -l $(($#-1)) \
    -x "$((MIDX - WIDTH/2))" \
    -y "$((MIDY - HEIGHT/2))" \
    -w $WIDTH \
    -h $HEIGHT \
    -fn "$FONT" \
    -ta c \
    -sa c \
    -p $TIMEOUT
