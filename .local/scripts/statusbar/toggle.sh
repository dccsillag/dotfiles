#!/bin/sh

if { pgrep polybar >/dev/null 2>&1; }
then
    pkill polybar
else
    polybar csillag
fi
