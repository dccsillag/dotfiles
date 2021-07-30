#!/bin/sh

while sleep 1
do
    cpu_usage="$(top -bn1 | grep 'Cpu(s)' | sed 's/.*, *\([0-9.]*\)%* id.*/\1/' | awk '{print 100 - $1}')"

    if [ "$(printf "%.0f" "$cpu_usage")" -gt 90 ]
    then
        color=tomato
    else
        color=''
    fi

    printf "^fg(%s)﬙ %3.0f%%\n" "$color" "$cpu_usage"
done
