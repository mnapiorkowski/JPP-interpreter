#!/bin/bash

EXT='.ris'

for dir in ./tests/{bad,good}/
do
    for file in $dir*$EXT
    do
        out=${file%$EXT}.out
        err=${file%$EXT}.err
        ./interpreter $file > $out 2> $err
    done

    for file in $dir*$EXT
    do
        out=${file%$EXT}.out
        err=${file%$EXT}.err
        ./interpreter $file > $out 2> $err
    done
done