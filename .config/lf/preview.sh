#!/bin/sh

FILE="$1"
WIDTH="$2"

display_image() {
    viu -s -w "$WIDTH" "$FILE"
    mediainfo "$FILE"
}

display_video() {
    mediainfo "$FILE"
}

case "$FILE" in
    *.tar*) tar tf "$FILE" ;;
    *.zip)  unzip -l "$FILE" ;;
    *.rar)  unrar l "$FILE" ;;
    *.7z)   7z l "$FILE" ;;
    *.pdf)  pdftotext "$FILE" - ;;
    *.png)  display_image ;;
    *.jpg)  display_image ;;
    *.jpeg) display_image ;;
    *.gif)  display_image ;;
    *.mp4)  display_video ;;
    *.webm) display_video ;;
    *)      highlight -O ansi "$FILE" || cat "$FILE" ;;
esac
