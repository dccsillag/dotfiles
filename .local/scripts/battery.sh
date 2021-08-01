#!/bin/sh

batpath="$(find /sys/class/power_supply/ -iname '*bat*')"

case "$1" in
    status) cat "$batpath"/status ;;
    charge) cat "$batpath"/capacity ;;
    *)      echo "bad command: must be either 'status' or 'charge'." 1>&2; exit 2 ;;
esac
