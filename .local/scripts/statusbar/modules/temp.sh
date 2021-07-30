#!/bin/sh

while sleep 1
do
    max_temp="$(sensors | grep '^.\+:.\+°C' | sed 's/^.\+: *+\([0-9.]\+\)°C.\+$/\1/' | sort -g | tail -1)"

    if [ "$(printf "%.0f" "$max_temp")" -gt 85 ]
    then
        color=tomato
    else
        color=''
    fi

    printf "﨎 ^fg(%s)%3.0f°C" "$color" "$max_temp"
done
