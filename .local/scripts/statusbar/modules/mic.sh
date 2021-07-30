#!/bin/sh

while sleep 0.1
do
    if { pacmd list-sources | grep 'name:\|muted:' | sed 'N;s/\n/ /' | grep -v monitor | grep -q "muted: no"; }
    then
        echo "^fg(tomato)"
    else
        echo ""
    fi
done
