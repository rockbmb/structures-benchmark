#!/bin/sh

[ -f "../dist/build/benchmark/benchmark" ] || { echo Compile the benchmark first!; exit 1; }
[ -n "$1" ] || { echo Missing name of the benchmark run!; exit 1; }

[ -d "$1" ] || mkdir "$1"

for c in IntMap IntSet Map Set
do
   for b in "Int " "IntegerSmall " "IntegerBig " "String 8" "String 50" "ByteString 8" "ByteString 50"
   do
     [ "$c" = IntMap -o "$c" = IntSet ] && [ "$b" != "Int " ] && continue

     for l in 1000 50000
     do
       (
       csv="$1/${c}_${b% *}${b#* }_$l.csv"
       csvm="${csv%.csv}m.csv"

       # Time benchmarks
       ../dist/build/benchmark/benchmark ${c}_$b $l -s 25 -g -u "$csv"

       # Memory benchmarks
       benchs=`tail -n+2 $csv | cut -d, -f1 | tr -d \"`
       case $b in
         Int*) repetitions=`expr 500000 / $l`;;
         *String*8) repetitions=`expr 100000 / $l`;;
         *String*50) repetitions=`expr 50000 / $l`;;
       esac

       head -1 "$csv" > "$csvm"
       for bench in $benchs
       do
         echo -n \"$bench\", >> "$csvm"
         ../dist/build/benchmark/benchmark ${c}_$b $l raw $bench $repetitions +RTS -t 2>&1 | sed -n "s/^.*ghc: \([0-9]*\) bytes.*$/\1/p" >> "$csvm"
       done
       ) &
     done
     wait

   done
done
