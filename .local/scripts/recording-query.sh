#!/bin/sh

SCREEN_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing your screen" | awk '{print $1}')
WINDOW_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing a window" | awk '{print $1}')

test -n "$SCREEN_SHARING" && test -z "$WINDOW_SHARING" && printf "<fc=#f29407>  </fc>"
test -z "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && printf "<fc=#f29407> 缾</fc>"
test -n "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && printf "<fc=#f29407> 缾   </fc>"

echo
