#!/bin/sh

while :
do
    ZOOM_LAUNCHER_ID=$(xwininfo -root -children | grep "Zoom - Free Account" | awk '{print $1}')
    ZOOM_MEETING_ID=$(xwininfo -root -children | grep "Zoom Meeting" | awk '{print $1}')

    if [ -n "$ZOOM_LAUNCHER_ID" ] && [ -n "$ZOOM_MEETING_ID" ]
    then
        xdo hide "$ZOOM_LAUNCHER_ID"
    fi
    if [ -n "$ZOOM_LAUNCHER_ID" ] && [ -z "$ZOOM_MEETING_ID" ]
    then
        xdo show "$ZOOM_LAUNCHER_ID"
    fi

    sleep 1
done
