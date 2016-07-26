#!/bin/sh

# This package _manually_ sets options for the containers
# repo used. We could extract the options from cabal file
# of from setup-config created by cabal configure.

set -e

FLAGS="-O2"

for c in Map Map/Base Map/Lazy Map/Strict Set IntMap IntMap/Base IntMap/Lazy IntMap/Strict IntSet StrictPair
do
  cat "$1/Data/$c.hs" | sed '
    1i\
{-# OPTIONS_GHC '"$FLAGS"' #-}
    s/Data\.IntMap/Container.IntMap/
    s/Data\.IntSet/Container.IntSet/
    s/Data\.Map/Container.Map/
    s/Data\.Set/Container.Set/
    s/Data\.StrictPair/Container.StrictPair/
  ' >../src/Container/$c.hs
done
