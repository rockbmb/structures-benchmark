{-# LANGUAGE CPP, BangPatterns #-}

module Variants.HashMap_String ( benchmark ) where

import qualified Container.HashMap as C
import qualified InputData.String
#include "../Driver.inc.hs"

#include "../Benchmark/Map-like.inc.hs"

benchmark = do
  strlenarg : args <- getArgs
  let strlen = read strlenarg
  withArgs args $ driver (InputData.String.asc strlen) (InputData.String.rnd strlen)