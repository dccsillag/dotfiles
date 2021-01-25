#!/bin/sh

set -e

usage() {
    echo "Usage: $0 [-h] -f <FORMAT> -i <INPUT> -o <OUTPUT>"
    echo "----------------------------------------------------------------------------"
    echo "  -h              Show this help message and exit"
    echo
    echo "  -f <FORMAT>     Format to generate"
    echo "  -i <FILE>       Input file (must be Markdown, to be preprocessed with gpp)"
    echo "  -o <FILE>       Output file"
}

if [ "$#" -lt 1 ]
then
    usage
    exit 0
fi

FORMAT=
OUTPUT=
INPUT=
while getopts hf:o:i: name
do
    case $name in
        f) FORMAT="$OPTARG" ;;
        o) OUTPUT="$OPTARG" ;;
        i) INPUT="$OPTARG"  ;;
        h) usage && exit 0  ;;
        ?) usage && exit 2  ;;
    esac
done
shift $((OPTIND - 1))

test -n "$FORMAT" || { echo "no format (-f) defined"; exit 2; }
test -n "$OUTPUT" || { echo "no output (-o) defined"; exit 2; }
test -n "$INPUT"  || { echo "no input (-i) defined";  exit 2; }

TMPFILE="$INPUT.tmp.md"

alias gpp_='gpp -x -U "#[" "]" " " ";" "]" "[" "]" "#" "" -M "#[" "]" " " ";" "]" "[" "]"'

gpp_ "$INPUT" -o "$TMPFILE"

panzer -f markdown -t "$FORMAT" -o "$OUTPUT" "$TMPFILE"

rm "$TMPFILE"
