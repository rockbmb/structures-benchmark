#!/bin/sh

for c in Map Set IntMap IntSet
do
  for i in Int IntegerSmall IntegerBig String ByteString
  do
    [ "$c" = IntMap -o "$c" = IntSet ] && [ "$i" != Int ] && continue

    case $i in
      Int | IntegerSmall | IntegerBig)
        addons="
#define FOLD_SUM";
        benchmark="
benchmark = driver (InputData.$i.asc) (InputData.$i.rnd)";;

      String | ByteString)
        addons="";
        benchmark="
benchmark = do
  strlenarg : args <- getArgs
  let strlen = read strlenarg
  withArgs args $ driver (InputData.$i.asc strlen) (InputData.$i.rnd strlen)";;
    esac

    cat >${c}_$i.hs <<EOF
{-# LANGUAGE CPP, BangPatterns #-}

module Variants.${c}_$i ( benchmark ) where

import qualified Container.$c as C
import qualified InputData.$i
#include "../Driver.inc.hs"
$addons
#include "../Benchmark/${c#Int}-like.inc.hs"
$benchmark
EOF
  done
done
