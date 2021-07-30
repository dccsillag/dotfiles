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
    [ -z "$1" ] \
        && file="$(find ~/static/backgrounds -type f | shuf -n 1)" \
        || file="$(echo "$1" | shuf -n 1)"
    feh --no-fehbg --bg-scale "$file"
    dunstify -a background-setter -u low -r 7971362 'Background Setter' 'Regenerating betterlockscreen cache...'
    betterlockscreen -u "$file"
    dunstify -a background-setter -u normal -r 7971362 'Background Setter' 'Done!'
}
DOWNTIME=2h

if [ "$1" = set ]
then
    set_background
elif [ "$1" = auto ]
then
    # Main loop
    while true
    do
        set_background

        sleep $DOWNTIME
    done
elif [ "$1" = choose ]
then
    files="$(sxiv -otr ~/static/backgrounds)"
    nfiles="$(echo "$files" | wc -l)"
    if [ "$nfiles" -eq 1 ]; then
        set_background "$files"
    elif [ "$nfiles" -gt 1 ]; then
        while true; do
            set_background "$files"
            sleep $DOWNTIME
        done
    fi
    set_background "$files"
else
    echo "no such command: $1. Must be 'set' or 'auto'" 1>&2
    exit 2
fi
