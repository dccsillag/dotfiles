#!/bin/sh

while :
do
    cpu_usage="$((100-$(vmstat 1 2 | tail -1 | awk '{print $15}')))"

    if [ "$(printf "%.0f" "$cpu_usage")" -gt 90 ]
    then
        color=tomato
    else
        color=''
    fi

    printf "^fg(%s)﬙ %3.0f%%\n" "$color" "$cpu_usage"
done
