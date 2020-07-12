#!/bin/sh

PASSNAME=$(zenity --entry --text 'Enter the password ID:')
test -z "$PASSNAME" && exit 0
PASS0=$(zenity --password --text 'Enter your password:')
test -z "$PASS0" && exit 0
PASS1=$(zenity --password --text 'Re-enter your password:')
test -z "$PASS1" && exit 0

if [ "$PASS0" != "$PASS1" ]
then
    zenity --error --text "Failed: The given passwords do not match."
    exit 1
fi

echo "$PASS0" | pass insert -m "$PASSNAME"
