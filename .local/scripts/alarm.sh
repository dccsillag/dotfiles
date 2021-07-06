#!/bin/sh

if [ $# != 2 ]
then
    notify-send 'invalid alarm; $# must equal 2.' 'alarm.sh <duration> "<message>"'
fi

sleep $1

paplay ~/.xmonad/audio-volume-change.wav
notify-send "Alarm done ($1)." "$2"
