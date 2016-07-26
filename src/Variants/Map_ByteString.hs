{-# LANGUAGE CPP, BangPatterns #-}

module Variants.Map_ByteString ( benchmark ) where

import qualified Container.Map as C
import qualified InputData.ByteString
#include "../Driver.inc.hs"

#include "../Benchmark/Map-like.inc.hs"

benchmark = do
  strlenarg : args <- getArgs
  let strlen = read strlenarg
  withArgs args $ driver (InputData.ByteString.asc strlen) (InputData.ByteString.rnd strlen)
