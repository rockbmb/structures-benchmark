#!/bin.sh

set -e

function usage {
  echo Usage: ghc.sh containers_repo ghc_repo result.csv
}

[ -d "$1" ] || { usage; exit 1; }
[ -d "$2" ] || { usage; exit 1; }
[ -n "$3" ] || { usage; exit 1; }

# 1) get ready the GHC source tree
[ -d "$2/libraries/containers/Data/IntMap/" ] || mkdir "$2/libraries/containers/Data/IntMap/"
[ -d "$2/libraries/containers/Data/Map/" ] || mkdir "$2/libraries/containers/Data/Map/"

cp "$1/containers.cabal"      "$2/libraries/containers/"
cp "$1/Data/IntMap.hs"        "$2/libraries/containers/Data/"
cp "$1/Data/IntMap/Base.hs"   "$2/libraries/containers/Data/IntMap/"
cp "$1/Data/IntMap/Lazy.hs"   "$2/libraries/containers/Data/IntMap/"
cp "$1/Data/IntMap/Strict.hs" "$2/libraries/containers/Data/IntMap/"
cp "$1/Data/IntSet.hs"        "$2/libraries/containers/Data/"
cp "$1/Data/Map.hs"           "$2/libraries/containers/Data/"
cp "$1/Data/Map/Base.hs"      "$2/libraries/containers/Data/Map/"
cp "$1/Data/Map/Lazy.hs"      "$2/libraries/containers/Data/Map/"
cp "$1/Data/Map/Strict.hs"    "$2/libraries/containers/Data/Map/"
cp "$1/Data/Set.hs"           "$2/libraries/containers/Data/"
cp "$1/Data/StrictPair.hs"    "$2/libraries/containers/Data/"

# 2) build GHC
(cd "$2" && make -j4)

# 3) collect the results
> "$3"
echo "File;Size" > "$3"
stat -c "ghc-stage2;%s" "$2/inplace/lib/ghc-stage2" >> "$3"
cp "$2/inplace/lib/ghc-stage2" /tmp/ghc-stage2
strip /tmp/ghc-stage2
stat -c "ghc-stage2 (stripped);%s" /tmp/ghc-stage2 >> "$3"
rm /tmp/ghc-stage2

echo -e "\nTest;Bytes allocated" >> "$3"
(cd "$2/testsuite/tests/perf/compiler/" && make)
for t in T1969 T3064 T3294 T4801 T5030
do
  echo -n "$t;" >> "$3"
  sed -n 's/^.*bytes allocated.*"\([0-9]*\)".*$/\1/;T;p' "$2/testsuite/tests/perf/compiler/$t.comp.stats" >> "$3"
done
