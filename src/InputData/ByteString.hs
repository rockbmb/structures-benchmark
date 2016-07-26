module InputData.ByteString ( asc
                            , rnd
                            ) where
import Data.ByteString.Char8 (ByteString, pack)
import qualified InputData.String

asc :: Int -> Int -> [ByteString]
asc strlen num = map pack $ InputData.String.asc strlen num

rnd :: Int -> Int -> [ByteString]
rnd strlen num = map pack $ InputData.String.rnd strlen num
