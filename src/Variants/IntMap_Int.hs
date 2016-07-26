{-# LANGUAGE CPP, BangPatterns #-}

module Variants.IntMap_Int ( benchmark ) where

import qualified Container.IntMap as C
import qualified InputData.Int
#include "../Driver.inc.hs"

#define FOLD_SUM
#include "../Benchmark/Map-like.inc.hs"

benchmark = driver (InputData.Int.asc) (InputData.Int.rnd)
