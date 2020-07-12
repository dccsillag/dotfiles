#!/bin/sh

LOW_BATTERY=25

SLEEP=5
LOWBATPATH=/tmp/low_battery

while true
do
    battery=$(upower -i "$(upower -e | grep BAT)" | grep percentage | sed 's/^.\+ \([0-9]\+\)%$/\1/)')

    if [ "$battery" -le "$LOW_BATTERY" ]
    then
        if ! [ -f "$LOWBATPATH" ]
        then
            notify-send -u critical 'Low battery!' "$battery%"
            touch "$LOWBATPATH"
        fi
    else
        test -f "$LOWBATPATH" && rm "$LOWBATPATH"
    fi

    sleep $SLEEP
done
