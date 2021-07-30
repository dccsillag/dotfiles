#!/bin/sh

while sleep 1
do
    mems=$(free -k | grep '^Mem:')
    total_mem=$(echo "$mems" | awk '{print $2}' )
    used_mem=$(echo "$mems" | awk '{print $3}' )
    cache_mem=$(echo "$mems" | awk '{print $6}')
    mem_usage=$((100*used_mem / total_mem))
    mem_cache=$((100*cache_mem / total_mem))

    if [ "$mem_usage" -gt 90 ]
    then
        color=tomato
    else
        color=''
    fi

    printf " ^fg(%s)%3d%%^fg() +%3d%%\n" "$color" "$mem_usage" "$mem_cache"
done
