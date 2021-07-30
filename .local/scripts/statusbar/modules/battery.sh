#!/bin/sh

while sleep 1
do
    battname="$(upower -e | grep BAT | head -1)"
    battpercent=$(upower -i "$battname" | grep percentage | sed 's/^.\+ \([0-9]\+\)%$/\1/')
    battstatus=$(upower -i "$battname" | grep state | sed 's/^.\+ \([-a-z]\+\)$/\1/')
    if [ "$battstatus" = fully-charged ]
    then
        icon=""
    elif [ "$battstatus" = discharging ]
    then
        test "$battpercent" -eq 100 && icon=""
        test "$battpercent" -lt 100 && icon=""
        test "$battpercent" -le  90 && icon=""
        test "$battpercent" -le  80 && icon=""
        test "$battpercent" -le  70 && icon=""
        test "$battpercent" -le  60 && icon=""
        test "$battpercent" -le  50 && icon=""
        test "$battpercent" -le  40 && icon=""
        test "$battpercent" -le  30 && icon=""
        test "$battpercent" -le  20 && icon=""
        test "$battpercent" -le  10 && icon=""
    elif [ "$battstatus" = charging ]
    then
        icon="ﮣ"
    else
        icon="?"
    fi

    if [ "$battpercent" -lt 10 ]
    then
        color="tomato"
    else
        color=""
    fi

    printf "  %s ^fg(%s)%2d%%\n" "$icon" "$color" "$battpercent"
done
