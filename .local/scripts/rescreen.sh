#!/bin/sh

# connected_monitors="$(xrandr | grep " connected " | awk '{ print $1 }')"
n_connected_monitors="$(xrandr | grep " connected " | wc -l)"

case $n_connected_monitors in
    1) mons -o ;;
    2) mons -s ;;
    ?) arandr ;;
esac
