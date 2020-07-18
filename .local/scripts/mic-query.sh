#!/bin/sh

if { pacmd list-sources | grep -q "muted: no"; }
then
    echo "<fc=#ff3333></fc>"
else
    echo ""
fi
