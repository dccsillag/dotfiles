#!/bin/sh

while sleep 2
do
    if pgrep -l vpn | grep -v "$(basename "$0")" | cut -d' ' -f 2 | grep '[v]pn' > /dev/null;
    then
        if pgrep -x ssh > /dev/null
        then
            color=yellow
        else
            color=''
        fi

        echo "^fg($color)嬨"
    else
        echo "^fg()"
    fi
done
