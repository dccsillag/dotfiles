#!/bin/sh

HEIGHT=22
FONT="FantasqueSansMono Nerd Font-12"
FONTWIDTH=9

textwidth() {
    printf '%s' "$1" | sed 's/\^[a-z]\+([^)]*)//g' | wc -m | awk "{printf \"%d\",\$1*$FONTWIDTH}"
}

append() {
    printf "%s" "$*"
}

block_sep() {
    append " ^fg() "
}

bar_left() {
    # CPU
    cpu_usage="$(top -bn1 | grep 'Cpu(s)' | sed 's/.*, *\([0-9.]*\)%* id.*/\1/' | awk '{print 100 - $1}')"
    append "﬙ "
    test "$(printf "%.0f" "$cpu_usage")" -gt 90 && append "^fg(red)"
    printf "%3.0f%%" "$cpu_usage"

    block_sep

    # RAM
    mems=$(free -k | grep '^Mem:')
    total_mem=$(echo "$mems" | awk '{print $2}' )
    used_mem=$(echo "$mems" | awk '{print $3}' )
    cache_mem=$(echo "$mems" | awk '{print $6}')
    mem_usage=$((100*used_mem / total_mem))
    mem_cache=$((100*cache_mem / total_mem))
    append " "
    test "$mem_usage" -gt 90 && append "^fg(red)"
    printf "%3d%%" "$mem_usage"
    append "^fg()"
    printf " +%3d%%" "$mem_cache"

    block_sep

    # Swap
    swaps=$(free -k | grep '^Swap:')
    total_swap=$(echo "$swaps" | awk '{print $2}' )
    used_swap=$(echo "$swaps" | awk '{print $3}' )
    swap_usage=$((100*used_swap / total_swap))
    append "易 "
    test "$swap_usage" -gt 80 && append "^fg(red)"
    printf "%3d%%" "$swap_usage"

    block_sep

    # Temperature
    max_temp="$(sensors | grep '^.\+:.\+°C' | sed 's/^.\+: *+\([0-9.]\+\)°C.\+$/\1/' | sort -g | tail -1)"
    append "﨎 "
    test "$(printf "%.0f" "$max_temp")" -gt 85 && append "^fg(red)"
    printf "%3.0f°C" "$max_temp"
}

bar_center() {
    # Date & Time
    append "$(date +'%a, %d/%m/%Y %H:%M:%S')"
}

bar_right() {
    # Do not disturb
    if [ -f /home/daniel/.dunst_paused ]
    then
        append "^fg(#00aa88)"
        append ""
        block_sep
    fi

    # Screen Sharing
    (
        SCREEN_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing your screen" | awk '{print $1}')
        WINDOW_SHARING=$(xwininfo -root -children | grep "[a-z.]\+ is sharing a window" | awk '{print $1}')

        test -n "$SCREEN_SHARING" && test -z "$WINDOW_SHARING" && append "^fg(#f29407)  "      && block_sep
        test -z "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && append "^fg(#f29407) 缾"      && block_sep
        test -n "$SCREEN_SHARING" && test -n "$WINDOW_SHARING" && append "^fg(#f29407) 缾   " && block_sep
    )

    # VPN
    if pgrep -l vpn | grep -v "$(basename "$0")" | cut -d' ' -f 2 | grep '[v]pn' > /dev/null;
    then
        if pgrep -x ssh > /dev/null
        then
            append "^fg(#ED8A00)"
        else
            append "^fg(#00AAEE)"
        fi
        append "嬨"
        block_sep
    fi

    # Mic
    if { pacmd list-sources | grep 'name:\|muted:' | sed 'N;s/\n/ /' | grep -v monitor | grep -q "muted: no"; }
    then
        append "^fg(red)"
        append ""
    else
        append "^fg(gray)"
        append ""
    fi

    block_sep
    block_sep

    # Battery
    battname="$(upower -e | grep BAT | head -1)"
    battpercent=$(upower -i "$battname" | grep percentage | sed 's/^.\+ \([0-9]\+\)%$/\1/')
    battstatus=$(upower -i "$battname" | grep state | sed 's/^.\+ \([-a-z]\+\)$/\1/')
    if [ "$battstatus" = fully-charged ]
    then
        icon=""
    elif [ "$battstatus" = discharging ]
    then
        test "$battpercent" -eq 100 && icon=""
        test "$battpercent" -lt 100 && icon=""
        test "$battpercent" -le  90 && icon=""
        test "$battpercent" -le  80 && icon=""
        test "$battpercent" -le  70 && icon=""
        test "$battpercent" -le  60 && icon=""
        test "$battpercent" -le  50 && icon=""
        test "$battpercent" -le  40 && icon=""
        test "$battpercent" -le  30 && icon=""
        test "$battpercent" -le  20 && icon=""
        test "$battpercent" -le  10 && icon=""
    elif [ "$battstatus" = charging ]
    then
        icon="ﮣ"
    else
        icon="?"
    fi
    append "$icon "
    test "$battpercent" -lt 10 && append "^fg(red)"
    printf "%2d%%" "$battpercent"
}

bar() {
    left=$(bar_left)
    append "^p(_LEFT)"
    append "$left"

    center=$(bar_center)
    append "^p(_CENTER)^p(-$(($(textwidth "$center")/2)))"
    append "$center"

    right=$(bar_right)
    append "^p(_RIGHT)^p(-$(textwidth "$right"))"
    append "$right"

    echo
}

while :
do
    bar
    sleep 0.5
done | dzen2 -xs 0 -x 0 -y 0 -h "$HEIGHT" -fn "$FONT" -dock -fg white
