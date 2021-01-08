#!/bin/sh

if pgrep -x ssh > /dev/null
then
    printf " <fc=#ED8A00>"
else
    printf " <fc=#00AAEE>"
fi

if pgrep -l vpn | grep -v "$(basename "$0")" | cut -d' ' -f 2 | grep '[v]pn' > /dev/null;
then
    printf "  嬨  "
fi
echo "</fc>"
