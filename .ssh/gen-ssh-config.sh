#!/bin/sh

alias gpp_='gpp -x -U "::[" "]" " " ";" "]" "[" "]" "#" "" -M "::[" "]" " " ";" "]" "[" "]"'

gpp_ "$HOME/.ssh/config.gpp" -o "$HOME/.ssh/config"
exit $?
