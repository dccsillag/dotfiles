#!/bin/sh

set -e

# yay -Syy > /dev/null

NUPS=$(yay -Qu 2> /dev/null | wc -l)

[ "$NUPS" -gt 0 ] && echo "<fc=#ffffff>  $NUPS</fc> "

exit 0
