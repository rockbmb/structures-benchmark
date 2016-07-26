module InputData.IntegerBig ( asc
                            , rnd
                            ) where
import System.Random (mkStdGen, randomRs)

asc :: Int -> [Integer]
asc len = [0, 2^64 .. 2^64 * (fromIntegral len - 1)]

rnd :: Int -> [Integer]
rnd len = take len $ randomRs (0, 1000 * 2^64 * (fromIntegral len - 1)) $ mkStdGen 1234
