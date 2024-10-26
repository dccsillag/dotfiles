#!/bin/sh -e

jj log --color=always --no-pager | tac
echo
jj st --color=always --no-pager
echo

NLINES=$(tput lines)
PADDING=$((2*NLINES/5))
for i in $(seq $PADDING); do echo; done
for i in $(seq $PADDING); do tput cuu1; done

zsh
exit $?
