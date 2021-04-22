#!/bin/sh

is_number() {
    echo "$1" | head -1 | grep '^[0-9]\+$' > /dev/null
    return $?
}

[ "$#" -lt 1 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ] && {
    echo "Usage:"
    echo "  $0 -l        # list sessions"
    echo "  $0 <COMMAND> # create a session"
    echo "  $0 -v <ID>   # view a session"
    echo "  $0 -d <ID>   # delete a session"
    echo "  $0 -c <ID>   # cancel a session (SIGINT)"
    echo "  $0 -t <ID>   # terminate a session (SIGTERM)"
    echo "  $0 -K <ID>   # kill -9 a session (SIGKILL)"
    exit 0
}

ROOTDIR="$HOME/.local/share/se"
mkdir -p "$ROOTDIR"

list_ids() {
    ls "$ROOTDIR" | grep '.cmd$' | sed 's|^\([-a-z0-9]\+\)\.cmd$|\1|'
}

get_id() {
    if is_number "$1"
    then
        list_ids | sort | tac | sed -n "$1p"
    else
        echo "$1"
    fi
}

for id in $(list_ids)
do
    [ -f "$ROOTDIR/$id.pid" ] || continue
    ( kill -0 "$(cat "$ROOTDIR/$id.pid")" 2> /dev/null ) || rm "$ROOTDIR/$id.pid"
done

is_number "$1" && {
    echo 'WARNING: The usage `se <NUMBER>` is deprecated and now has no meaning.'
    echo '         Use `se -v '"$1"'` instead.'
    exit 1
}

case "$1" in
    -l) for id in $(list_ids)
        do
            [ -f "$ROOTDIR/$id.pid" ] && STATUS=running || STATUS=done
            echo "[$STATUS] $id  --  $(cat "$ROOTDIR/$id.date")\t$(cat "$ROOTDIR/$id.cmd")"
        done | sort | tac | cat -n | tac | sed 's/^\(.\+\t.\+\)\t\(.\+\)/\1\n\t\2/'
        ;;
    -v) "${PAGER:-less}" "$ROOTDIR/$(get_id "$2").out" ;;
    -d)
        id="$(get_id "$2")"
        [ -f "$ROOTDIR/$id.pid" ] && {
            echo "Session is running, and thus cannot be deleted: '$id'."
            exit 1
        }
        echo "Are you sure you want to delete this session?"
        echo "  UUID: $id"
        echo "  COMMAND: $(cat "$ROOTDIR/$id.cmd")"
        echo "  STARTED RUNNING: $(cat "$ROOTDIR/$id.date")"
        echo
        printf '[yn] '
        read -r yn
        case "$yn" in
            [Yy]*)  echo "Deleting session '$id'" ;;
            [Nn]*)  echo "Aborting."; exit 0 ;;
            *)      echo "Answer was not 'yes' nor 'no'; aborting"; exit 1 ;;
        esac

        set -x
        rm "$ROOTDIR/$id.cmd"
        rm "$ROOTDIR/$id.out"
        rm "$ROOTDIR/$id.date"
        ;;
    -c) kill -SIGINT  "$(cat "$ROOTDIR/$(get_id "$2").pid")" ;;
    -t) kill -SIGTERM "$(cat "$ROOTDIR/$(get_id "$2").pid")" ;;
    -K) kill -SIGKILL "$(cat "$ROOTDIR/$(get_id "$2").pid")" ;;
    -*) echo "Bad flag: $1. See \`se -h\`." ;;
    *)  id=$(uuidgen)
        echo "$(date)" > "$ROOTDIR/$id.date"
        nohup "$@" > "$ROOTDIR/$id.out" 2>&1 &
        pid="$!"
        echo "$pid" > "$ROOTDIR/$id.pid"
        echo "$*" > "$ROOTDIR/$id.cmd"

        echo "Running in the background."
        echo "  COMMAND: $*"
        echo "  PID: $pid"
        echo "  UUID: $id"
        ;;
esac
