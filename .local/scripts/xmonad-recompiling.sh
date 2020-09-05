#!/bin/sh

( ps aux | grep "[x]monad --recompile" > /dev/null ) && echo " recompiling..." || echo
