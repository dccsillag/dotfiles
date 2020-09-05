#!/bin/sh

while :
do
    ZOOM_LAUNCHER_ID=$(xwininfo -root -children | grep "Zoom - Free Account" | awk '{print $1}')
    ZOOM_MEETING_ID=$(xwininfo -root -children | grep "Zoom Meeting" | awk '{print $1}')

    [ -n "$ZOOM_LAUNCHER_ID" ] && [ -n "$ZOOM_MEETING_ID" ] && xdo hide "$ZOOM_LAUNCHER_ID"
    [ -n "$ZOOM_LAUNCHER_ID" ] && [ -z "$ZOOM_MEETING_ID" ] && xdo show "$ZOOM_LAUNCHER_ID"

    sleep 1
done
