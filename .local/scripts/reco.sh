#!/bin/sh

set -e

usage() {
    printf "Usage: %s [-h] [-s|-S|-w] [-a] [-A] [-o out.mp4]\n" "$0"
    echo "--------------------------------------------------------------------------------"
    echo "  -h        Show this help message and exit"
    echo
    echo "  -s        Record the whole screen"
    echo "  -S        Record the whole screen, except for the statusbar (Polybar only)"
    echo "  -w        Record a single window"
    echo
    echo "  -m        Record microphone audio via PulseAudio"
    echo "  -M        Record microphone audio via ALSA"
    echo "  -a        Record output audio via PulseAudio"
    echo "  -A        Record output audio via ALSA"
    echo
    echo "  -f        Force overwrite"
    echo "  -o <FILE> Specify an output file to write (default: out.mp4)"
}

if [ "$#" -lt 1 ]
then
    usage
    exit 0
fi

outfile=out.mp4
framerate=60
while getopts hsSwmMaAfo: name
do
    case $name in
        s)
            echo ". Recording the whole screen"
            videoargs="-f x11grab -video_size 1920x1080 -framerate $framerate -i $DISPLAY"
            ;;
        S)
            echo ". Recording the whole screen, except Polybar"
            polybarwininfo=$(xwininfo -id "$(xdotool search --class polybar | head -n 1)")
            polybarheight=$(echo "$polybarwininfo" | grep '^  Height:' | sed 's/^.\+:\s*\([0-9]\+\)$/\1/')
            videoargs="-f x11grab -video_size 1920x$((1080-polybarheight)) -framerate $framerate -i $DISPLAY+0,$polybarheight"
            ;;
        w)
            wininfo=$(xwininfo)
            winname=$(echo "$wininfo" | grep '^xwininfo: Window id: ' | sed 's/^xwininfo: Window id: [0-9a-z]\+ "\(.\+\)"/\1/')
            echo ". Recording window '$winname'"
            x=$(echo "$wininfo" | grep '^  Absolute upper-left X:' | sed 's/^.\+:\s*\([0-9]\+\)$/\1/')
            y=$(echo "$wininfo" | grep '^  Absolute upper-left Y:' | sed 's/^.\+:\s*\([0-9]\+\)$/\1/')
            w=$(echo "$wininfo" | grep '^  Width:' | sed 's/^.\+:\s*\([0-9]\+\)$/\1/')
            h=$(echo "$wininfo" | grep '^  Height:' | sed 's/^.\+:\s*\([0-9]\+\)$/\1/')
            videoargs="-f x11grab -video_size ${w}x${h} -framerate $framerate -i $DISPLAY+$x,$y"
            ;;
        a)
            # FIXME: increase volume
            pamonitor=$(pactl list sources short | awk '{print($2)}' | grep 'monitor$' | tail -n 1)
            echo ". Recording audio with PulseAudio (source=$pamonitor)"
            audioargs="-f pulse -i $pamonitor"
            ;;
        A)
            # TODO: ALSA audio
            ;;
        m)
            echo ". Recording microphone audio with PulseAudio"
            pasource=$(pactl list sources short | awk '{print($2)}' | grep -v 'monitor$' | tail -n 1)
            micargs="-f pulse -i $pasource"
            ;;
        M)
            echo ". Recording microphone audio"
            micargs="-f alsa -i default"
            ;;
        f)
            forceargs="-y"
            ;;
        o)
            outfile="$OPTARG"
            ;;
        h)
            usage
            exit 0
            ;;
        ?)
            usage
            exit 2
            ;;
    esac
done
shift $((OPTIND - 1))

set -x

# shellcheck disable=SC2086 # this word splitting is intentional
ffmpeg \
    -hide_banner -loglevel warning \
    $videoargs \
    $micargs \
    $audioargs \
    -c:v h264 -preset ultrafast -c:a aac \
    $forceargs \
    "$outfile"
