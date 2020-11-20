#!/bin/sh

case "$1" in
    proc) percent=$(nvidia-smi dmon -s u -c 1 | tail -1 | awk '{print $2}')
          if [ "$percent" -lt 5 ]
          then
              color='green'
          elif [ "$percent" -lt 50 ]
          then
              color='yellow'
          else
              color='red'
          fi
          # printf "ﯟ <fc=$color>%3d</fc>%%\n" "$percent"
          printf " <fc=$color>%3d</fc>%%\n" "$percent"
          ;;
    mem)  percent=$(nvidia-smi dmon -s u -c 1 | tail -1 | awk '{print $3}')
          if [ "$percent" -lt 20 ]
          then
              color='green'
          elif [ "$percent" -lt 80 ]
          then
              color='yellow'
          else
              color='red'
          fi
          printf "舘 <fc=$color>%3d</fc>%%\n" "$percent"
          ;;
    *)    echo must be \'proc\' or \'mem\'
          exit 2
          ;;
esac
