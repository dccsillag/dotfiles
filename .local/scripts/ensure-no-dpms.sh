#!/bin/sh

while true
do
    xset q | grep 'Monitor is On' && xset -dpms
    sleep 10
done
