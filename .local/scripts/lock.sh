#!/bin/sh

pkill picom
slock
picom --glx-fshader-win "$(cat ~/.config/picom-opacity-shader.glsl)"
