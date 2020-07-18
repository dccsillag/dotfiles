#!/bin/sh

if [ $(ps aux | grep [s]sh | wc -l) -gt 1 ]
then
    printf "<fc=#ED8A00>"
else
    printf "<fc=#00AAEE>"
fi

if ps aux | grep [v]pn > /dev/null;
then
    printf "VPN"
fi
echo "</fc>"
