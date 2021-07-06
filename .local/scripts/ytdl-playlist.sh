#!/bin/sh

# youtube-dl -o "%(playlist)s/%(playlist_index)s. %(title)s.%(ext)s" --abort-on-error -f 'bestaudio[ext=m4a]' $@
# youtube-dl -o "%(playlist)s/%(playlist_index)s. %(title)s.%(ext)s" --abort-on-error -f 'bestaudio[ext=webm]' $@
youtube-dl -o "%(playlist)s/%(playlist_index)s. %(title)s.%(ext)s" --abort-on-error -f bestaudio $@
