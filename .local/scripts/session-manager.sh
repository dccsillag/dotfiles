#!/bin/sh

[ "$#" -lt 1 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ] && {
    echo "Usage:"
    echo "  $0 -l        # list sessions"
    echo "  $0 <COMMAND> # create a session"
    echo "  $0 <ID>      # attach a session, readonly"
    echo "  $0 <ID> -e   # attach a session, interactive"
    exit 0
}

is_number() {
    echo "$1" | head -1 | grep '^[0-9]\+$' > /dev/null
    return $?
}

list_ids() {
    ls ~/.abduco | grep '.name$' | sed 's|^\([-a-z0-9]\+\)\.name$|\1|'
}

for id in $(list_ids)
do
    [ -S "$HOME/.abduco/$id@$(hostname)" ] || rm "$HOME/.abduco/$id.name"
done

if [ "$1" = '-l' ]; then
    for id in $(list_ids)
    do
        cat "$HOME/.abduco/$id.name"
    done | cat -n
elif is_number "$1"; then
    [ "$2" = "-e" ] && READONLY= || READONLY=-r
    abduco $READONLY -a "$(list_ids | sed "$1"'q;d')"
else
    uuid=$(uuidgen)

    echo "$*" > "$HOME/.abduco/$uuid.name"
    abduco -f -c "$uuid" dvtm "$*; echo Done running. Press ENTER to exit.; read -r"
fi
