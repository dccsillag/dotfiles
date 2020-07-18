#!/bin/sh

if pgrep xmobar | grep -v $$
then
    killall xmobar
else
    xmobar
fi
