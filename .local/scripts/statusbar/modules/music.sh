#!/bin/sh

while sleep 1
do
    if mcm get active
    then
        queuetext="$(mcm get queuepos)/$(mcm get ninqueue)"
        # ninqueue="$(printf "%2d" "$ninqueue")"
        case $(mcm get status) in
            paused)  echo "  $queuetext  "; ;;
            playing) echo " 契 $queuetext  "; ;;
            waiting) echo  "..$queuetext  "; ;;
            empty) ;;
        esac
    else
        echo "^fg()"
    fi
done
