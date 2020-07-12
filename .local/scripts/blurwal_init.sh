#!/bin/sh

# Kill already running blurwal instances
killall -q blurwal

# Launch it
blurwal -m 1 -s 5 -b 7 -i dzen,Rofi

echo "Blurwal launched"
