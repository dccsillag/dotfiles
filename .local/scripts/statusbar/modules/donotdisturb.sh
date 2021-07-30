#!/bin/sh

while sleep 0.2
do
    if [ -f /home/daniel/.dunst_paused ]
    then
        echo ""
    else
        echo "^fg()"
    fi
done
