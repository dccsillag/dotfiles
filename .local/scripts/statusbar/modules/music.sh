#!/bin/sh

while sleep 1
do
    mcm get active && {
        queuetext="$(mcm get queuepos)/$(mcm get ninqueue)"
        # ninqueue="$(printf "%2d" "$ninqueue")"
        case $(mcm get status) in
            paused)  echo "  $queuetext  "; ;;
            playing) echo " 契 $queuetext  "; ;;
            waiting) echo  "..$queuetext  "; ;;
            empty) ;;
        esac
    }
done
