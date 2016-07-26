{-# LANGUAGE CPP, BangPatterns #-}

module Variants.Map_IntegerSmall ( benchmark ) where

import qualified Container.Map as C
import qualified InputData.IntegerSmall
#include "../Driver.inc.hs"

#define FOLD_SUM
#include "../Benchmark/Map-like.inc.hs"

benchmark = driver (InputData.IntegerSmall.asc) (InputData.IntegerSmall.rnd)
