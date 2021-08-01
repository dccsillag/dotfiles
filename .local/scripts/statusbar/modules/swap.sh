#!/bin/sh

while sleep 1
do
    swaps=$(swapon --show --bytes --raw | grep '^/swapfile\>\|zram')
    total_swap=$(echo "$swaps" | cut -d' ' -f3)
    used_swap=$(echo "$swaps" | cut -d' ' -f4)
    swap_usage=$((100*used_swap / total_swap))

    if [ "$swap_usage" -gt 80 ]
    then
        color=tomato
    else
        color=''
    fi

    printf "易 ^fg(%s)%3d%%\n" "$color" "$swap_usage"
done
