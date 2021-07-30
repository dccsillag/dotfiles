#!/bin/sh

if pgrep dzen
then
    pkill dzen
else
    ~/.local/scripts/statusbar/main.py
fi
