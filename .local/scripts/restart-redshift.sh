#!/bin/sh

pkill redshift

while pgrep redshift > /dev/null
do
    sleep 0.1
done

nohup redshift >/dev/null 2>&1 &
