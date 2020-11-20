#!/bin/sh

if { pacmd list-sources | grep 'name:\|muted:' | sed 'N;s/\n/ /' | grep -v monitor | grep -q "muted: no"; }
then
    echo "<fc=#ff3333></fc>"
    # echo " <fc=#ff4444>MIC</fc>"
else
    echo ""
    # echo
fi
