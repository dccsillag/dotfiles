#!/bin/sh

is_number() {
    echo "$1" | head -1 | grep '^[0-9]\+$' > /dev/null
    return $?
}

if is_number "$1"; then
    printf '\e]11;#%s\a' "$1"
else
    printf '\e]11;%s\a' "$1"
fi
