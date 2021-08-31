#!/bin/sh

while sleep 2
do
    if { pgrep -f 'battery-monitor$' > /dev/null 2>&1; }
    then
        echo "^fg()"
    else
        echo "^fg(tomato)BATTERY-MONITOR NOT RUNNING"
    fi
done
