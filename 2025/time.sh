#!/usr/bin/bash
for file in src/bin/*; do
    EXE=`basename $file .rs`
    echo Building $EXE...
    cargo build --release --bin $EXE
    echo Running $EXE...
    time target/release/$EXE
    echo
done
echo
echo Run all
echo
time find target/release -type f -iname '2025-12-[0-9][0-9]' -exec {} \;