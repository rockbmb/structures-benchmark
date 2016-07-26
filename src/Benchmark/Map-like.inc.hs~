create list = C.fromList (zip list list)
benchs = [ ("insert", bench_insert)
         , ("lookup0", bench_lookup0)
         , ("lookup50", bench_lookup50)
         , ("lookup100", bench_lookup100)
         , ("delete", bench_delete)
         , ("alter-insert", bench_alterinsert)
         , ("alter-delete", bench_alterdelete)
         , ("union", bench_union)
         , ("difference", bench_difference)
         , ("intersection", bench_intersection)
         , ("fold-list", bench_foldlist)
         , ("fold-sum", bench_foldsum)
         ]

bench_insert (list, _, _, _) = go C.empty list
  where go !c [] = ()
        go !c (x:xs) = go (C.insert x x c) xs

bench_lookup100 (list, c, _, _) = go c list
  where go c [] = ()
        go c (x:xs) = C.lookup x c `seq` go c xs

bench_lookup50 (list, _, _, c_odd) = go c_odd list
  where go c [] = ()
        go c (x:xs) = C.lookup x c `seq` go c xs

bench_lookup0 (list, _, _, c_odd) = go c_odd list
  where go c [] = ()
        go c [_] = ()
        go c (_:x:xs) = C.lookup x c `seq` go c xs

bench_delete (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (C.delete x c) xs

bench_alterinsert (list, _, _, _) = go C.empty list
  where go !c [] = ()
        go !c (x:xs) = go (C.alter (const $ Just x) x c) xs

bench_alterdelete (list, c, _, _) = go c list
  where go !c [] = ()
        go !c (x:xs) = go (C.alter (const $ Nothing) x c) xs

bench_union (_, _, c_even, c_odd) = C.union c_even c_odd `seq` ()

bench_difference (_, c, c_even, _) = C.difference c c_even `seq` ()

bench_intersection (_, c, c_even, _) = C.intersection c c_even `seq` ()

bench_foldlist (_, c, _, _) = force $ C.foldr (:) [] c
  where force list = length list `seq` ()

bench_foldsum (_, c, _, _) = C.foldl' (+) 0 c `seq` ()
