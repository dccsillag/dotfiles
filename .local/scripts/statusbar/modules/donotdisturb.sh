#!/bin/sh

while sleep 0.2
do
    if [ "$(dunstctl is-paused)" = true ]
    then
        echo ""
    else
        echo "^fg()"
    fi
done
