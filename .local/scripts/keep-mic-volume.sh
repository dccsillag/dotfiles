#!/bin/sh

SOURCE="alsa_input.hw_0_6"
VOLUME="40%"

while true
do
    pactl set-source-volume "$SOURCE" "$VOLUME"
    sleep 2
done
