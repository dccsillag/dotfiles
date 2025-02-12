#!/bin/sh

# Setup compose key, caps->control/escape
xmodmap ~/.config/Xmodmap
# xcape -e "Control_L=Escape;Alt_L=Tab;Control_R=Shift_L|asterisk"
# xcape -e "Control_L=Escape;Super_R=Tab;Control_R=Shift_L|asterisk"
pgrep xcape || xcape -e "Control_L=Escape;Alt_L=Tab"

# Enable push-to-talk
xbindkeys -f ~/.config/xbindkeys/config
# Remove key repeat for the push-to-talk key
xset r rate 300 50
xset -r 86

# Disable tap-to-click
# synclient MaxTapTime=0
# Disable edge scrolling
# xinput set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Edge Scrolling" 0 0 0
# Set natural scrolling
# xinput set-prop "PS/2 Synaptics TouchPad" "Synaptics Scrolling Distance" -113 113
# Enable side scrolling
# synclient HorizTwoFingerScroll=1

# Set screensaver timeout to 1 hour
xset s 3600 3600
# .local/scripts/ensure-no-dpms.sh &
# setterm -blank 0 -powerdown 0

# Warn low battery
# batmon --warn-percentage 25 --panic-percentage 8 BAT0 &
battery-monitor &

# Set background image
background-setter set &

# Launch redshift
# redshift &

# Enable gestures using touchscreen
# easystroke enable &

# Disable touch
# disable-touch

# /usr/lib/geoclue-2.0/demos/agent &

xinput set-prop 'SYNA32A1:00 06CB:CE17 Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'DELL0B24:00 27C6:0D42 Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'DELL0B24:00 27C6:0D42 Touchpad' 'libinput Accel Speed' -0.2
