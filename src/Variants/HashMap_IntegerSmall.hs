{-# LANGUAGE CPP, BangPatterns #-}

module Variants.HashMap_IntegerSmall ( benchmark ) where

import qualified Container.HashMap as C
import qualified InputData.IntegerSmall
#include "../Driver.inc.hs"

#define FOLD_SUM
#include "../Benchmark/Map-like.inc.hs"

benchmark = driver (InputData.IntegerSmall.asc) (InputData.IntegerSmall.rnd)
