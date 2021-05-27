#!/bin/sh

is_number() {
    echo "$1" | head -1 | grep '^[0-9ABCDEFabcdef]\+$' > /dev/null
    return $?
}

if is_number "$1"; then
    printf '\033]11;#%s\a' "$1"
else
    printf '\033]11;%s\a' "$1"
fi
