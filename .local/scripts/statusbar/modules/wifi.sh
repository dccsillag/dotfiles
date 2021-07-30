#!/bin/sh

while sleep 2
do
    if { nm-online > /dev/null 2>&1; }
    then
        echo "直"
    else
        echo "^fg(tomato)睊"
    fi
done
