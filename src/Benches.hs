{-# LANGUAGE CPP           #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Benches where

import           System.Environment              (getArgs, withArgs)
import           Control.DeepSeq                 (NFData)
import           Criterion.Main
import           Data.ByteString.Char8           (ByteString, pack)
import           Data.Foldable                   (foldr')
import           Data.Hashable                   (Hashable)
import qualified Data.Map.Lazy                   as ML
import qualified Data.Map.Strict                 as MS
import qualified Data.IntMap.Lazy                as IL
import qualified Data.IntMap.Strict              as IS
import qualified Data.HashMap.Lazy               as HL
import qualified Data.HashMap.Strict             as HS
import           System.Random                   (mkStdGen, randomRs)

genBS :: (Int -> Int -> [String]) -> Int -> Int -> [ByteString]
genBS genS strlen num = map pack $ genS strlen num

ascBS :: Int -> Int -> [ByteString]
ascBS = genBS ascS

rndBS :: Int -> Int -> [ByteString]
rndBS = genBS rndS

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

#define insert(NAME, FUN) \
  NAME (list, _, _, _) = go mempty list \
    where go !c [] = c ;\
          go !c (x:xs) = go (FUN x x c) xs \

insert(bench_insert_MS, MS.insert)
insert(bench_insert_ML, ML.insert)
insert(bench_insert_IS, IS.insert)
insert(bench_insert_IL, IL.insert)
insert(bench_insert_HS, HS.insert)
insert(bench_insert_HL, HL.insert)

#define lookup(NAME, FUN) \
  NAME (list, c, _, _) = go c list \
    where go c [] = c ;\
          go c (x:xs) = FUN x c `seq` go c xs \

lookup(bench_lookup_MS, MS.lookup)
lookup(bench_lookup_ML, ML.lookup)
lookup(bench_lookup_IS, IS.lookup)
lookup(bench_lookup_IL, IL.lookup)
lookup(bench_lookup_HS, HS.lookup)
lookup(bench_lookup_HL, HL.lookup)

#define delete(NAME, FUN) \
  NAME (list, c, _, _) = go c list \
    where go !c [] = c ;\
          go !c (x:xs) = go (FUN x c) xs \

delete(bench_delete_MS, MS.delete)
delete(bench_delete_ML, ML.delete)
delete(bench_delete_IS, IS.delete)
delete(bench_delete_IL, IL.delete)
delete(bench_delete_HS, HS.delete)
delete(bench_delete_HL, HL.delete)

{-#define alter_insert(Name, Fun) \
  Name (list, _, _, _) = go mempty list \
    where go !c [] = c ;\
          go !c (x:xs) = go (Fun (const $ Just x) x c) xs \

alter_insert(bench_alter_insert_MS, MS.alter_insert)
alter_insert(bench_alter_insert_ML, ML.alter_insert)
alter_insert(bench_alter_insert_IS, IS.alter_insert)
alter_insert(bench_alter_insert_IL, IL.alter_insert)
alter_insert(bench_alter_insert_HS, HS.alter_insert)
alter_insert(bench_alter_insert_HL, HL.alter_insert)-}

{-#define alter_delete(NAME, FUN) \
  NAME (list, c, _, _) = go c list \
    where go !c [] = () ;\
          go !c (x:xs) = go (FUN (const $ Nothing) x c) xs \
}

alter_delete(bench_alter_delete_MS, MS.alter_delete)
alter_delete(bench_alter_delete_ML, ML.alter_delete)
alter_delete(bench_alter_delete_IS, IS.alter_delete)
alter_delete(bench_alter_delete_IL, IL.alter_delete)
alter_delete(bench_alter_delete_HS, HS.alter_delete)
alter_delete(bench_alter_delete_HL, HL.alter_delete)-}

#define union(NAME) \
  bench_union_NAME (_, _, c_even, c_odd) = NAME.union c_even c_odd \

union(MS)
union(ML)
union(IS)
union(IL)
union(HS)
union(HL)

#define difference(NAME) \
  bench_difference_NAME (_, c, c_even, _) = NAME.difference c c_even \

difference(MS)
difference(ML)
difference(IS)
difference(IL)
difference(HS)
difference(HL)

#define intersection(NAME) \
  bench_intersection_NAME (_, c, c_even, _) = NAME.intersection c c_even \

intersection(MS)
intersection(ML)
intersection(IS)
intersection(IL)
intersection(HS)
intersection(HL)

#define foldlist(NAME) \
  bench_foldlist_NAME (_, c, _, _) = force $ NAME.foldr (:) [] c \
    where force list = length list `seq` ()

foldlist(MS)
foldlist(ML)
foldlist(IS)
foldlist(IL)
foldlist(HS)
foldlist(HL)

folderL a k accum = accum + (a + k)
folderR k accum a = accum + (a + k)

#define foldWK(NAME1, NAME2, NAME3, NAME4, FUN1, FUN2, FUN3, FUN4) \
  NAME1 (_, c, _, _) = FUN1 folderL 0 c `seq` () ;\
  NAME2 (_, c, _, _) = FUN2 folderL 0 c `seq` () ;\
  NAME3 (_, c, _, _) = FUN3 folderR 0 c `seq` () ;\
  NAME4 (_, c, _, _) = FUN4 folderR 0 c `seq` () ;\

foldWK(bench_foldWK_MS1, bench_foldWK_MS2, bench_foldWK_MS3, bench_foldWK_MS4, MS.foldlWithKey, MS.foldlWithKey', MS.foldrWitKey, MS.foldrWithKey')
foldWK(bench_foldWK_ML1, bench_foldWK_ML2, bench_foldWK_ML3, bench_foldWK_ML4, ML.foldlWithKey, ML.foldlWithKey', ML.foldrWitKey, ML.foldrWithKey')
foldWK(bench_foldWK_IS1, bench_foldWK_IS2, bench_foldWK_IS3, bench_foldWK_IS4, IS.foldlWithKey, IS.foldlWithKey', IS.foldrWitKey, IS.foldrWithKey')
foldWK(bench_foldWK_IL1, bench_foldWK_IL2, bench_foldWK_IL3, bench_foldWK_IL4, IL.foldlWithKey, IL.foldlWithKey', IL.foldrWitKey, IL.foldrWithKey')
foldWK(bench_foldWK_HS1, bench_foldWK_HS2, bench_foldWK_HS3, bench_foldWK_HS4, HS.foldlWithKey, HS.foldlWithKey', HS.foldrWitKey, HS.foldrWithKey')
foldWK(bench_foldWK_HL1, bench_foldWK_HL2, bench_foldWK_HL3, bench_foldWK_HL4, HL.foldlWithKey, HL.foldlWithKey', HL.foldrWitKey, HL.foldrWithKey')

foldrer :: Int -> Int -> Int
foldrer x y = x + y + 1
bench_foldRS (l, _, _, _) = foldr' foldrer 0 l `seq` ()
bench_foldRL (l, _, _, _) = foldr foldrer 0 l `seq` ()

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

foldBenches num =
    defaultMain [
        bgroup "foldr" [ intBench num "lazy foldr" MS.fromList bench_foldRL "foldr"
                       , intBench num "strict foldr" MS.fromList bench_foldRS "foldr"
                       ]
        ]

foldWKBenches num =
    defaultMain [
        bgroup "folder" [ bench_foldWKMS bench_foldWK_MS1 "lazy foldlWK"
                        , bench_foldWKMS bench_foldWK_MS2 "strict foldrWK"
                        , bench_foldWKMS bench_foldWK_MS3 "lazy foldrWK"
                        , bench_foldWKMS bench_foldWK_MS4 "strict foldrWK"
                        , bench_foldWKML bench_foldWK_ML1 "lazy foldlWK"
                        , bench_foldWKML bench_foldWK_ML2 "strict foldlWK"
                        , bench_foldWKML bench_foldWK_ML3 "lazy foldrWK"
                        , bench_foldWKML bench_foldWK_ML4 "strict foldrWK"
                        ]
        ]
    where bench_foldWKMS foldWK str = intBench num ("map strict " ++ str) MS.fromList foldWK "folder"
          bench_foldWKML foldWK str = intBench num ("map lazy " ++ str) ML.fromList foldWK "folder"

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
        "folder" -> do
            foldWKBenches
        "foldr" -> do
            foldBenches

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
