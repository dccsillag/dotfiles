#!/bin/sh

while :
do
    WINDOW_IDS=$(xwininfo -root -children | grep "[a-z.]\+ is sharing \(a window\|your screen\)" | awk '{print $1}')

    for window_id in $WINDOW_IDS
    do
        xdo hide "$window_id"
    done

    sleep 1
done
