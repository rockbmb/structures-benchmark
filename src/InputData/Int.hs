module InputData.Int ( asc
                     , rnd
                     ) where
import System.Random (mkStdGen, randomRs)

asc :: Int -> [Int]
asc len = [1..len]

rnd :: Int -> [Int]
rnd len = take len $ randomRs (0,1000 * len) $ mkStdGen 1234
