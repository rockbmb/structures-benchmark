module Main where

import System.Environment (getArgs, withArgs)

import qualified Variants.IntMap_Int as IM_I
import qualified Variants.Map_ByteString as M_BS
import qualified Variants.Map_IntegerBig as M_IB
import qualified Variants.Map_IntegerSmall as M_IS
import qualified Variants.Map_Int as M_I
import qualified Variants.Map_String as M_S
import qualified Variants.HashMap_ByteString as S_BS
import qualified Variants.HashMap_IntegerBig as S_IB
import qualified Variants.HashMap_IntegerSmall as S_IS
import qualified Variants.HashMap_Int as S_I
import qualified Variants.HashMap_String as S_S

main = do
    argss <- getArgs
    case argss of
        variant : args ->
            withArgs args $
                case variant of
                  "IntMap_Int"       -> IM_I.benchmark
                  "Map_ByteString"   -> M_BS.benchmark
                  "Map_IntegerBig"   -> M_IB.benchmark
                  "Map_IntegerSmall" -> M_IS.benchmark
                  "Map_Int"          -> M_I.benchmark
                  "Map_String"       -> M_S.benchmark
                  "HashMap_ByteString"   -> S_BS.benchmark
                  "HashMap_IntegerBig"   -> S_IB.benchmark
                  "HashMap_IntegerSmall" -> S_IS.benchmark
                  "HashMap_Int"          -> S_I.benchmark
                  "HashMap_String"       -> S_S.benchmark
        _ -> error "my friend, main needs arguments"