#!/bin/zsh
rustc -C opt-level=3 --edition 2021 -C target-cpu=native "$1/main.rs" -o "$1/main"
