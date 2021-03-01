#!/bin/sh

[ "$#" -lt 1 ] && echo 'bad params for open' && return 1

for file in "$@"; do
    nohup xdg-open "$1" > /dev/null 2>&1 &
done
