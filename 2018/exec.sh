#!/bin/bash

go build -o "$1/main" "$1/main.go"
OUTPUT=$(./$1/main $1/input)
rm $1/main
echo "${OUTPUT}"
