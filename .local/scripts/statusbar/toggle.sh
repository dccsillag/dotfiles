#!/bin/sh

if { eww windows | grep '^\*bar-window$' >/dev/null 2>&1; }
then
    eww close bar-window
else
    eww open bar-window
fi
