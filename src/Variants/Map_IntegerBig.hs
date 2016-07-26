{-# LANGUAGE CPP, BangPatterns #-}

module Variants.Map_IntegerBig ( benchmark ) where

import qualified Container.Map as C
import qualified InputData.IntegerBig
#include "../Driver.inc.hs"

#define FOLD_SUM
#include "../Benchmark/Map-like.inc.hs"

benchmark = driver (InputData.IntegerBig.asc) (InputData.IntegerBig.rnd)
