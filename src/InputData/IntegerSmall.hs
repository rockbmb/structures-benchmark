module InputData.IntegerSmall ( asc
                              , rnd
                              ) where
import System.Random (mkStdGen, randomRs)

asc :: Int -> [Integer]
asc len = [1..fromIntegral len]

rnd :: Int -> [Integer]
rnd len = take len $ randomRs (0,1000 * fromIntegral len) $ mkStdGen 1234
