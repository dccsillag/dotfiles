#!/bin/sh

if xmonad --recompile
then
    xmonad --restart
    notify-send -u low XMonad "Restarted."
else
    notify-send -u critical XMonad "Compilation failed."
fi
