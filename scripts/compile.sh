#!/bin/sh

if [ "$1" = none ]
then
  echo Using default containers
  sh system_containers.sh
else
  echo Using containers from $1
  sh custom_containers.sh $1 || { echo Containers repo not found in $1; exit 1; }
fi
shift

(cd .. && cabal clean && cabal configure "$@" && cabal build && strip dist/build/benchmark/benchmark )
