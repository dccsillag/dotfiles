#!/bin/sh

DOWNTIME=1m

while true
do
    file="$(find ~/static/backgrounds -type f | shuf -n 1)"
    feh --no-fehbg --bg-scale "$file"

    sleep $DOWNTIME
done
