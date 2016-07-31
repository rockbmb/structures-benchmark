{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           System.Environment                    (getArgs, withArgs)
import           Control.DeepSeq                       (NFData)
import           Criterion.Main
import           Data.ByteString.Char8                 (ByteString, pack)
import           Data.Hashable                         (Hashable)
import qualified Data.Map.Lazy                   as ML
import qualified Data.Map.Strict                 as MS
import qualified Data.IntMap.Lazy                as IL
import qualified Data.IntMap.Strict              as IS
import qualified Data.HashMap.Lazy               as HL
import qualified Data.HashMap.Strict             as HS
import qualified Data.Edison.Assoc.AssocList     as AL
import qualified Data.Edison.Assoc.PatriciaLoMap as PM
import qualified Data.Edison.Assoc.StandardMap   as SM
import qualified Data.Edison.Assoc.TernaryTrie   as TT
import           System.Random                         (mkStdGen, randomRs)

ascBS :: Int -> Int -> [ByteString]
ascBS strlen num = map pack $ ascS strlen num

rndBS :: Int -> Int -> [ByteString]
rndBS strlen num = map pack $ rndS strlen num

ascS :: Int -> Int -> [String]
ascS strlen num = take num $ iterate (snd . inc) $ replicate strlen 'a'
  where inc [] = (True, [])
        inc (c:cs) = case inc cs of (True, cs') | c == 'z'  -> (True, 'a' : cs')
                                                | otherwise -> (False, succ c : cs')
                                    (False, cs')            -> (False, c : cs')

rndS :: Int -> Int -> [String]
rndS strlen num = take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
    where
      split cs = case splitAt strlen cs of (str, cs') -> str : split cs'

asc :: Int -> [Int]
asc len = [1..len]

rnd :: Int -> [Int]
rnd len = take len $ randomRs (0,1000 * len) $ mkStdGen 1234

bench_insert_MS (list, _, _, _) = go MS.empty list
   where go !c [] = c
         go !c (x:xs) = go (MS.insert x x c) xs
bench_insert_ML (list, _, _, _) = go ML.empty list
   where go !c [] = c
         go !c (x:xs) = go (ML.insert x x c) xs
bench_insert_IS (list, _, _, _) = go IS.empty list
   where go !c [] = c
         go !c (x:xs) = go (IS.insert x x c) xs
bench_insert_IL (list, _, _, _) = go IL.empty list
   where go !c [] = c
         go !c (x:xs) = go (IL.insert x x c) xs
bench_insert_HS (list, _, _, _) = go HS.empty list
   where go !c [] = c
         go !c (x:xs) = go (HS.insert x x c) xs
bench_insert_HL (list, _, _, _) = go HL.empty list
   where go !c [] = c
         go !c (x:xs) = go (HL.insert x x c) xs

bench_lookup_MS (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = MS.lookup x c `seq` go c xs
bench_lookup_ML (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = ML.lookup x c `seq` go c xs
bench_lookup_IS (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = IS.lookup x c `seq` go c xs
bench_lookup_IL (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = IS.lookup x c `seq` go c xs
bench_lookup_HL (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = HL.lookup x c `seq` go c xs
bench_lookup_HS (list, c, _, _) = go c list
  where go c [] = c
        go c (x:xs) = HS.lookup x c `seq` go c xs

bench_delete_MS (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (MS.delete x c) xs
bench_delete_ML (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (ML.delete x c) xs
bench_delete_IS (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (IS.delete x c) xs
bench_delete_IL (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (IL.delete x c) xs
bench_delete_HS (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (HS.delete x c) xs
bench_delete_HL (list, c, _, _) = go c list
  where go !c [] = c
        go !c (x:xs) = go (HL.delete x c) xs

bench_alterinsert_MS (list, _, _, _) = go MS.empty list
  where go !c [] = c
        go !c (x:xs) = go (MS.alter (const $ Just x) x c) xs
bench_alterinsert_ML (list, _, _, _) = go ML.empty list
  where go !c [] = c
        go !c (x:xs) = go (ML.alter (const $ Just x) x c) xs
bench_alterinsert_IS (list, _, _, _) = go IS.empty list
  where go !c [] = c
        go !c (x:xs) = go (IS.alter (const $ Just x) x c) xs
bench_alterinsert_IL (list, _, _, _) = go IL.empty list
  where go !c [] = c
        go !c (x:xs) = go (IL.alter (const $ Just x) x c) xs
bench_alterinsert_HS (list, _, _, _) = go HS.empty list
  where go !c [] = c
        go !c (x:xs) = go (HS.alter (const $ Just x) x c) xs
bench_alterinsert_HL (list, _, _, _) = go HL.empty list
  where go !c [] = c
        go !c (x:xs) = go (HL.alter (const $ Just x) x c) xs

bench_alterdelete_MS (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (MS.alter (const $ Nothing) x c) xs
bench_alterdelete_ML (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (ML.alter (const $ Nothing) x c) xs
bench_alterdelete_IL (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (IL.alter (const $ Nothing) x c) xs
bench_alterdelete_IS (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (IS.alter (const $ Nothing) x c) xs
bench_alterdelete_HL (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (HL.alter (const $ Nothing) x c) xs
bench_alterdelete_HS (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (HS.alter (const $ Nothing) x c) xs

bench_union_MS (_, _, c_even, c_odd) = MS.union c_even c_odd
bench_union_ML (_, _, c_even, c_odd) = ML.union c_even c_odd
bench_union_IS (_, _, c_even, c_odd) = IS.union c_even c_odd
bench_union_IL (_, _, c_even, c_odd) = IL.union c_even c_odd
bench_union_HS (_, _, c_even, c_odd) = HS.union c_even c_odd
bench_union_HL (_, _, c_even, c_odd) = HL.union c_even c_odd

bench_difference_MS (_, c, c_even, _) = MS.difference c c_even
bench_difference_ML (_, c, c_even, _) = ML.difference c c_even
bench_difference_IS (_, c, c_even, _) = IS.difference c c_even
bench_difference_IL (_, c, c_even, _) = IL.difference c c_even
bench_difference_HS (_, c, c_even, _) = HS.difference c c_even
bench_difference_HL (_, c, c_even, _) = HL.difference c c_even

bench_intersection_MS (_, c, c_even, _) = MS.intersection c c_even
bench_intersection_ML (_, c, c_even, _) = ML.intersection c c_even
bench_intersection_IS (_, c, c_even, _) = IS.intersection c c_even
bench_intersection_IL (_, c, c_even, _) = IL.intersection c c_even
bench_intersection_HS (_, c, c_even, _) = HS.intersection c c_even
bench_intersection_HL (_, c, c_even, _) = HL.intersection c c_even

bench_foldlist_MS (_, c, _, _) = force $ MS.foldr (:) [] c
  where force list = length list `seq` ()
bench_foldlist_ML (_, c, _, _) = force $ ML.foldr (:) [] c
  where force list = length list `seq` ()
bench_foldlist_IS (_, c, _, _) = force $ IS.foldr (:) [] c
  where force list = length list `seq` ()
bench_foldlist_IL (_, c, _, _) = force $ IL.foldr (:) [] c
  where force list = length list `seq` ()
bench_foldlist_HS (_, c, _, _) = force $ HS.foldr (:) [] c
  where force list = length list `seq` ()
bench_foldlist_HL (_, c, _, _) = force $ HL.foldr (:) [] c
  where force list = length list `seq` ()

bench_criterion bnch =
  map (\(name, method, input) ->
          bench name (nf method input))
      bnch

create fromL l = fromL $ zip l l

{-benchMaker :: NFData b =>
              Int
           -> (Int -> [a])
           -> (Int -> [a])
           -> ([(a,a)] -> t)
           -> (String, ([a], t, t, t) -> b)
           -> [Benchmark]-}
benchMaker len asce rand fromL benchmark =
    let lenarg = show len
        asc_list = asce len
        asc_data = (asc_list
                   , create fromL asc_list
                   , create fromL (even asc_list)
                   , create fromL (odd asc_list))
        rnd_list = rand len
        rnd_data = (rnd_list
                   , create fromL rnd_list
                   , create fromL (even rnd_list)
                   , create fromL (odd rnd_list))
        benchs' = (\(name, method) ->
                      [(name ++ "_asc" ++ lenarg, method, asc_data),
                       (name ++ "_rnd" ++ lenarg, method, rnd_data)])
                  benchmark
    in bench_criterion benchs'
    where
     even []       = []
     even [_]      = []
     even (_:x:xs) = x : even xs

     odd []       = []
     odd [x]      = [x]
     odd (x:_:xs) = x : odd xs

intBench (num, asc_, rnd_) name fromL method method_name =
    bgroup name $
    benchMaker num asc_ rnd_ fromL ((,) method_name method)

ins = "insert"
lkup = "lookup"
del = "delete"
altins = "alter insert"
altdel = "alter delete"
uni = "union"
diff = "difference"
inter = "intersection"
fold = "fold"

insertBenches num =
    defaultMain [
        bgroup ins [ intBench num "map strict" MS.fromList bench_insert_MS ins
                   , intBench num "map lazy" ML.fromList bench_insert_ML ins
                   , intBench num "intmap strict" IS.fromList bench_insert_IS ins
                   , intBench num "intmap lazy" IL.fromList bench_insert_IL ins
                   , intBench num "hashmap strict" HS.fromList bench_insert_HS ins
                   , intBench num "hashmap lazy" HL.fromList bench_insert_HL ins
                   ]
        ]

lookupBenches num =
    defaultMain [
        bgroup lkup [ intBench num "map strict" MS.fromList bench_lookup_MS lkup
                    , intBench num "map lazy" ML.fromList bench_lookup_ML lkup
                    , intBench num "intmap strict" IS.fromList bench_lookup_IS lkup
                    , intBench num "intmap lazy" IL.fromList bench_lookup_IL lkup
                    , intBench num "hashmap strict" HS.fromList bench_lookup_HS lkup
                    , intBench num "hashmap lazy" HL.fromList bench_lookup_HL lkup
                    ]
        ]

deleteBenches num =
    defaultMain [
        bgroup del [ intBench num "map strict" MS.fromList bench_delete_MS del
                   , intBench num "map lazy" ML.fromList bench_delete_ML del
                   , intBench num "intmap strict" IS.fromList bench_delete_IS del
                   , intBench num "intmap lazy" IL.fromList bench_delete_IL del
                   , intBench num "hashmap strict" HS.fromList bench_delete_HS del
                   , intBench num "hashmap lazy" HL.fromList bench_delete_HL del
                   ]
        ]

alterInsBenches num =
    defaultMain [
        bgroup altins [ intBench num "map strict" MS.fromList bench_alterinsert_MS altins
                      , intBench num "map lazy" ML.fromList bench_alterinsert_ML altins
                      , intBench num "intmap strict" IS.fromList bench_alterinsert_IS altins
                      , intBench num "intmap lazy" IL.fromList bench_alterinsert_IL altins
                      , intBench num "hashmap strict" HS.fromList bench_alterinsert_HS altins
                      , intBench num "hashmap lazy" HL.fromList bench_alterinsert_HL altins
                      ]
        ]

alterDelBenches num =
    defaultMain [
        bgroup altdel [ intBench num "map strict" MS.fromList bench_alterdelete_MS altdel
                      , intBench num "map lazy" ML.fromList bench_alterdelete_ML altdel
                      , intBench num "intmap strict" IS.fromList bench_alterdelete_IS altdel
                      , intBench num "intmap lazy" IL.fromList bench_alterdelete_IL altdel
                      , intBench num "hashmap strict" HS.fromList bench_alterdelete_HS altdel
                      , intBench num "hashmap lazy" HL.fromList bench_alterdelete_HL altdel
                   ]
        ]

unionBenches num =
    defaultMain [
        bgroup uni [ intBench num "map strict" MS.fromList bench_union_MS uni
                   , intBench num "map lazy" ML.fromList bench_union_ML uni
                   , intBench num "intmap strict" IS.fromList bench_union_IS uni
                   , intBench num "intmap lazy" IL.fromList bench_union_IL uni
                   , intBench num "hashmap strict" HS.fromList bench_union_HS uni
                   , intBench num "hashmap lazy" HL.fromList bench_union_HL uni
                   ]
        ]

differenceBenches num =
    defaultMain [
        bgroup diff [ intBench num "map strict" MS.fromList bench_difference_MS diff
                    , intBench num "map lazy" ML.fromList bench_difference_ML diff
                    , intBench num "intmap strict" IS.fromList bench_difference_IS diff
                    , intBench num "intmap lazy" IL.fromList bench_difference_IL diff
                    , intBench num "hashmap strict" HS.fromList bench_difference_HS diff
                    , intBench num "hashmap lazy" HL.fromList bench_difference_HL diff
                    ]
        ]

intersectionBenches num =
    defaultMain [
        bgroup inter [ intBench num "map strict" MS.fromList bench_intersection_MS inter
                     , intBench num "map lazy" ML.fromList bench_intersection_ML inter
                     , intBench num "intmap strict" IS.fromList bench_intersection_IS inter
                     , intBench num "intmap lazy" IL.fromList bench_intersection_IL inter
                     , intBench num "hashmap strict" HS.fromList bench_intersection_HS inter
                     , intBench num "hashmap lazy" HL.fromList bench_intersection_HL inter
                     ]
        ]

foldListBenches num =
    defaultMain [
        bgroup fold [ intBench num "map strict" MS.fromList bench_insert_MS fold
                    , intBench num "map lazy" ML.fromList bench_foldlist_ML fold
                    , intBench num "intmap strict" IS.fromList bench_foldlist_IS fold
                    , intBench num "intmap lazy" IL.fromList bench_foldlist_IL fold
                    , intBench num "hashmap strict" HS.fromList bench_foldlist_HS fold
                    , intBench num "hashmap lazy" HL.fromList bench_foldlist_HL fold
                    ]
        ]

x & f = f x

benchDecider benchfun =
    case benchfun of
        "insert" -> do
            insertBenches
        "lookup" -> do
            lookupBenches
        "delete" -> do
            deleteBenches
        "alter_insert" -> do
            alterInsBenches
        "alter_delete" -> do
            alterDelBenches
        "union" -> do
            unionBenches
        "difference" -> do
            differenceBenches
        "intersection" -> do
            intersectionBenches
        "fold" -> do
            foldListBenches

main = do
  n : benchtype : benchfun : args <- getArgs
  let num = (read n) :: Int
  {-let pair@(num ,(ascend, random)) = (,) <$> ((read n) :: Int) <*> helper
      helper = case args of
                  [] -> (asc,rnd)
                  [strl] -> let strlen = (read strl) :: Int
                            in case benchtype of
                                   "bytestring" -> (ascBS strlen, rndBS strlen)-}
  case benchtype of
      "int" -> do
          (num, asc, rnd) & benchDecider benchfun
      {-"bytestring" -> do
          (num, ascBS, rndBS) & benchDecider benchfun-}
