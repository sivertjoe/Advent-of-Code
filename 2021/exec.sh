#!/bin/zsh
rustc -C opt-level=3 --edition 2021 "$1/main.rs" -o "$1/main"
cd $1
OUTPUT=$(./main)
rm main
echo "${OUTPUT}"
