#!/bin/sh

DOWNTIME=1h

# Kill other running instances
for pid in $(pidof -x "$0")
do
    if [ "$pid" != $$ ]
    then
        echo "kill $pid"
        kill "$pid"
    fi
done

# Main loop
while true
do
    file="$(find ~/static/backgrounds -type f | shuf -n 1)"
    feh --no-fehbg --bg-scale "$file"

    sleep $DOWNTIME
done
