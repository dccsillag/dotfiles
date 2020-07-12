#!/bin/sh

PASSNAME=$(zenity --entry --text 'Enter the password ID:')

pass generate "$PASSNAME"
