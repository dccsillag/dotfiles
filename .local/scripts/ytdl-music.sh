#!/bin/sh

youtube-dl -o "%(title)s.%(ext)s" --abort-on-error -f 'bestaudio[ext=m4a]' $@
