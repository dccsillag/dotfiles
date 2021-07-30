#!/bin/sh

LOW_BATTERIES=$(echo 25; echo 15; echo 10; echo 5)
HIBERNATE_BAT=9

SLEEP=5
LOWBATPATH=/tmp/low_battery

rm -f "$LOWBATPATH"

while true
do
    battery=$(upower -i "$(upower -e | grep BAT)" | grep percentage | sed 's/^.\+ \([0-9]\+\)%$/\1/')

    for LOW_BATTERY in $LOW_BATTERIES
    do
        if [ "$battery" -le "$LOW_BATTERY" ]
        then
            if ! [ -f "$LOWBATPATH$LOW_BATTERY" ]
            then
                notify-send -u critical 'Low battery!' "$battery%"
                touch "$LOWBATPATH$LOW_BATTERY"
            fi
        else
            test -f "$LOWBATPATH$LOW_BATTERY" && rm "$LOWBATPATH$LOW_BATTERY"
        fi
    done

    if [ "$battery" -le "$HIBERNATE_BAT" ]
    then
        if ! [ -f "$LOWBATPATH"hibernate ]
        then
            systemctl hibernate
            touch "$LOWBATPATH"hibernate
        fi
    else
        test -f "$LOWBATPATH"hibernate && rm "$LOWBATPATH"hibernate
    fi

    sleep $SLEEP
done
