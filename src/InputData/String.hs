module InputData.String ( asc
                        , rnd
                        ) where
import System.Random (mkStdGen, randomRs)

asc :: Int -> Int -> [String]
asc strlen num = take num $ iterate (snd . inc) $ replicate strlen 'a'
  where inc [] = (True, [])
        inc (c:cs) = case inc cs of (True, cs') | c == 'z'  -> (True, 'a' : cs')
                                                | otherwise -> (False, succ c : cs')
                                    (False, cs')            -> (False, c : cs')

rnd :: Int -> Int -> [String]
rnd strlen num = take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'
