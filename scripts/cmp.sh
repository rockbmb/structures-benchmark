#!/bin/sh

perl ${0%cmp.sh}cmp.pl "$1" "$2" | column -nts\; | less -S
