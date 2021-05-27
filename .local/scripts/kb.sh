#!/bin/sh

pkill xcape

setxkbmap "$@"

xmodmap ~/.config/Xmodmap
xcape -e "Control_L=Escape;Super_R=Tab"
