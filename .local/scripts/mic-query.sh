#!/bin/sh

if { pacmd list-sources | grep -q "muted: no"; }
then
    # echo "<fc=#ff3333></fc>"
    echo " <fc=#ff4444>MIC</fc>"
else
    # echo ""
    echo
fi
