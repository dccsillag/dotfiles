#!/bin/sh

cd ~/.xmonad || { notify-send -u critical XMonad 'Could not cd into `~/.xmonad`!!'; exit 1; }

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

notify "Starting..."

SUCESS_FILE="$HOME/.xmonad/recompilation-success"

{ stack build 2>&1 && touch "$SUCESS_FILE"; } | ( while IFS="" read -r line
do
    ( echo "$line" | grep -q "^[A-Za-z\[]" ) && notify "$line"
    # just to close the bracket: ]
done )

if [ -f "$SUCESS_FILE" ]
then
    xmonad --restart
    notify "Successfully restarted." normal
else
    notify "Compilation failed." critical
fi

rm -f "$SUCESS_FILE"
