#!/bin/sh

LOW_BATTERIES=$(echo 20; echo 15; echo 10; echo 5; echo 2)
SUSPEND_BAT=1

SLEEP=10
LOWBATPATH=/tmp/low_battery

rm -f "$LOWBATPATH"

while true
do
    battery=$(battery charge)

    for LOW_BATTERY in $LOW_BATTERIES
    do
        if [ "$battery" -le "$LOW_BATTERY" ]
        then
            if ! [ -f "$LOWBATPATH$LOW_BATTERY" ]
            then
                # notify-send -u critical 'Low battery!' "$battery%"
                eww open low-battery --duration 4.5s
                touch "$LOWBATPATH$LOW_BATTERY"
            fi
        else
            test -f "$LOWBATPATH$LOW_BATTERY" && rm "$LOWBATPATH$LOW_BATTERY"
        fi
    done

    if [ "$battery" -le "$SUSPEND_BAT" ]
    then
        if ! [ -f "$LOWBATPATH"suspend ]
        then
            systemctl suspend
            touch "$LOWBATPATH"suspend
        fi
    else
        test -f "$LOWBATPATH"suspend && rm "$LOWBATPATH"suspend
    fi

    sleep $SLEEP
done
