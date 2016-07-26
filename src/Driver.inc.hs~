import Criterion.Main (bench, defaultMain, whnf)
import System.Environment (getArgs, withArgs)

bench_criterion benchs =
  defaultMain $ map (\(name, method, input) -> bench name (whnf method input)) benchs

bench_raw benchs = do
  bencharg : numreparg : _ <- getArgs
  let [(_, method, input)] = filter (\(name, _, _) -> name == bencharg) benchs
      numrep = read numreparg
  go numrep method input

 where
  go !0 _      _     = return ()
  go !n method input = method input `seq` go (n-1) method input

driver asc rnd = do
  lenarg : args <- getArgs

  let len = read lenarg
      asc_list = asc len
      asc_data = (asc_list, create asc_list, create (even asc_list), create (odd asc_list))
      rnd_list = rnd len
      rnd_data = (rnd_list, create rnd_list, create (even rnd_list), create (odd rnd_list))
      benchs' = concatMap ( \(name, method) -> [(name ++ "_asc" ++ lenarg, method, asc_data),
                                                (name ++ "_rnd" ++ lenarg, method, rnd_data)]
                          ) benchs

  case args of
    "raw" : args' -> withArgs args' $ bench_raw benchs'
    args'         -> withArgs args' $ bench_criterion benchs'

 where
  even []       = []
  even [_]      = []
  even (_:x:xs) = x : even xs

  odd []       = []
  odd [x]      = [x]
  odd (x:_:xs) = x : odd xs
