#!/bin/sh

batpath="$(find /sys/class/power_supply/ -iname '*bat*')"

has_battery() {
    [ -n "$batpath" ]
}

case "$1" in
    status)
        if has_battery
        then
            cat "$batpath"/status
        else
            echo "Charging"
        fi ;;
    charge)
        if has_battery
        then
            cat "$batpath"/capacity
        else
            echo "100"
        fi ;;
    *)      echo "bad command: must be either 'status' or 'charge'." 1>&2; exit 2 ;;
esac
