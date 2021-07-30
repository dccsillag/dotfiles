#!/bin/sh

while sleep 2
do
    SCREEN_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing your screen" | awk '{print $1}')
    WINDOW_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing a window" | awk '{print $1}')

    test -n "$SCREEN_SHARING" && test -z "$WINDOW_SHARING" && echo "^fg(darkorange)  " && continue
    test -z "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && echo "^fg(darkorange) 缾" && continue
    test -n "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && echo "^fg(darkorange) 缾   " && continue

    echo "^fg()"
done
