#!/bin/sh

for c in Map Set IntMap IntSet
do
  cat >../src/Container/$c.hs <<EOF
module Container.$c (module Data.$c) where

import Data.$c
EOF
done
