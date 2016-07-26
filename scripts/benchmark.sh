#!/bin/sh

[ -n "$1" ] || { echo Missing name of the benchmark run!; exit 1; }
[ -n "$2" ] || { echo Missing name of the containers repo or \'none\'!; exit 1; }

dir="$1-`date +%y%m%d`"
[ -d "$dir" ] || mkdir "$dir"
repo="$2"
shift 2

sh compile.sh "$repo" "$@"
sh run.sh "$dir"
perl collect.pl "$dir"
