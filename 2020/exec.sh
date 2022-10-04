#!/bin/bash

docker build -t aocexec .

docker run -v "$(pwd)"/$1:/out aocexec
# docker run -v /Users/sivert/Documents/Advent-of-Code/2020/day_01:/out aocexec
