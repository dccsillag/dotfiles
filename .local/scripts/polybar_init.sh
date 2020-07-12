#!/bin/sh

# Kill already running bar instances
killall -q polybar

# Launch bar
echo "---" > /tmp/polybar.log
polybar $1 >>/tmp/polybar.log 2>&1 &

echo "Bars launched"
