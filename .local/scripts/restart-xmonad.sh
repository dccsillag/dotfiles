#!/bin/sh

# Random, but unique ID:
MSGID=483972

notify() {
    if [ -n "$2" ]
    then
        urgency="$2"
    else
        urgency=low
    fi
    dunstify -a xmonad-recompilation -r "$MSGID" -u "$urgency" "Recompiling XMonad" "$1"
}

xmonad --restart

if [ -f "$SUCESS_FILE" ]
then
    xmonad --restart
    notify "Successfully restarted." normal
else
    notify "Compilation failed." critical
fi

rm -f "$SUCESS_FILE"
