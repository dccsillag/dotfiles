#!/bin/sh

killall redshift

while pgrep redshift > /dev/null
do
    sleep 0.1
done

redshift -o -P

nohup redshift >/dev/null 2>&1 &
