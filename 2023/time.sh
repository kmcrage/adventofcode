#!/usr/bin/bash
DIR=`realpath $PWD`
for dir in $PWD/2023-12-*; do
    cd $dir
    echo $dir
    for file in ./*.go; do
        EXE=`basename $file .go`
        go build $file
        time ./$EXE
        rm ./$EXE
    done
    cd $DIR
done