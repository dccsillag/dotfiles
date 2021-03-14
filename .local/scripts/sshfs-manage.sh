#!/bin/sh

[ "$#" -ne 2 ] && { echo "Usage: $0 <COMMAND> <HOST>"; exit 2; }

COMMAND="$1"
HOSTNAME="$2"

MOUNTPATH="/mnt/daniel/sshfs/$HOSTNAME"

setup() {
    set -x
    mkdir -p "$MOUNTPATH"
}

case "$COMMAND" in
    init)   test -d "$MOUNTPATH" && exit 0; set -x; mkdir -p "$MOUNTPATH";
            sshfs -o reconnect,ServerAliveInterval=60,follow_symlinks -C "$HOSTNAME": "$MOUNTPATH" ;;
    deinit) test -d "$MOUNTPATH" || exit 0; set -x;
            test -d "$MOUNTPATH" && umount "$MOUNTPATH" && rmdir "$MOUNTPATH" ;;
    get)    test -d "$MOUNTPATH" && realpath "$MOUNTPATH" ;;
    *)      echo "Bad command: '$COMMAND'. Must be one of 'init', 'deinit' or 'get'." ;;
esac
