#!/bin/sh

is_number() {
    echo "$1" | head -1 | grep '^[0-9ABCDEFabcdef]\+$' > /dev/null
    return $?
}

# If running from rsync, do nothing.
{ ps -o ppid= | xargs -L1 ps -o comm= -p | grep rsync >/dev/null 2>&1; } && exit 0

if is_number "$1"; then
    printf '\033]11;#%s\a' "$1"
else
    printf '\033]11;%s\a' "$1"
fi
