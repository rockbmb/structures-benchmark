#!/bin/bash

for i in {insert,lookup,delete,alter_insert,alter_delete,union,difference,intersection,fold}; do
    ./tester 10000 int $i --output {$i}10000.html --regress allocated:iters +RTS -T
    ./tester 100000 int $i --output {$i}100000.html --regress allocated:iters +RTS -T
done

for i in *.html; do
    FILE=$(basename "$i")
    DIR=~/Desktop/HTMLFinalResults
    mv -i -t "$DIR" -- "$i"
done
