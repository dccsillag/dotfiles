#!/bin/sh

pkill xwacomcalibrate
pkill xdotool

case "$1" in
    up)     xwacomcalibrate -d         ;;
    down)   xwacomcalibrate -d -r half ;;
    cw)     xwacomcalibrate -d -r cw   ;;
    ccw)    xwacomcalibrate -d -r ccw  ;;
    screen) xwacomcalibrate -f 1       ;;
esac
