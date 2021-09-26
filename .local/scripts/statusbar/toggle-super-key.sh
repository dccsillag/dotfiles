#!/bin/sh -x

TRACKING_FILE=/tmp/super-key-mode-$USER
SUPER_KEY=133

xdo key_release -k "$SUPER_KEY"
if [ -f "$TRACKING_FILE" ]
then
    rm "$TRACKING_FILE"
else
    xdo key_press -k "$SUPER_KEY"
    touch "$TRACKING_FILE"
fi
