#!/bin/sh

pulseaudio -k

sleep 0.5

sudo -A alsa-restart

pulseaudio -D

amixer sset Master 100% -c 0
amixer sset Master unmute -c 0

notify-send "Probably fixed audio."
