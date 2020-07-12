#!/bin/sh

if pgrep --exact screenkey
then
    killall screenkey
else
    screenkey --timeout 0.5
fi
