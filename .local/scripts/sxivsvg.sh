#!/bin/sh

if [ $# != 1 ]
then
    echo "$0: number of arguments must be 1"
    exit
fi

convert "$1" > "$1.png"

sxiv "$1"
