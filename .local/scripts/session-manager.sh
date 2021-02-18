#!/bin/sh

[ "$#" -lt 1 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ] && {
    echo "Usage:"
    echo "  $0 -l        # list sessions"
    echo "  $0 <COMMAND> # create a session"
    echo "  $0 <ID>      # attach a session"
    exit 0
}

is_number() {
    echo "$1" | head -1 | grep '^[0-9]\+$' > /dev/null
    return $?
}

if [ "$1" = '-l' ]; then
    abduco | head -1
    abduco | sed '1d' | cat -n
elif is_number "$1"; then
    test "$2" = "-e" && READONLY= || READONLY=-r
    abduco $READONLY -a "$(abduco | sed '1d' | rev | cut -f1 | rev | sed "$1"'q;d')"
else
    abduco -f -c "$*" "$*"
fi
