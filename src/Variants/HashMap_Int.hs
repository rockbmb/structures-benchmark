{-# LANGUAGE CPP, BangPatterns #-}

module Variants.HashMap_Int ( benchmark ) where

import qualified Container.HashMap as C
import qualified InputData.Int
#include "../Driver.inc.hs"

#define FOLD_SUM
#include "../Benchmark/Map-like.inc.hs"

benchmark = driver (InputData.Int.asc) (InputData.Int.rnd)
