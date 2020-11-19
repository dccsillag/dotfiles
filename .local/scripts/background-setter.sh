#!/bin/sh

# Kill other running instances
for pid in $(pidof -x "$0")
do
    if [ "$pid" != $$ ]
    then
        echo "kill $pid"
        kill "$pid"
    fi
done

set_background() {
    file="$(find ~/static/backgrounds -type f | shuf -n 1)"
    feh --no-fehbg --bg-scale "$file"
}

if [ "$1" = set ]
then
    set_background
elif [ "$1" = auto ]
then
    DOWNTIME=1h

    # Main loop
    while true
    do
        set_background

        sleep $DOWNTIME
    done
else
    echo "no such command: $1. Must be 'set' or 'auto'" 1>&2
    exit 2
fi
