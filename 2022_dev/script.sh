#!/bin/bash
# YEAR=$1
# input_string=$(awk -v year="$YEAR" '$0 ~ year' README.md)
# hex_color=$(echo "$input_string" | grep -o -E '[0-9a-fA-F]{6}' | cut -d')' -f1)
#
# decimal_color=$(( 16#${hex_color} ))
# decimal_color=$(( decimal_color - 119048 ))
# hex_color=$( printf "%x\n" ${decimal_color} )
#
# number=$(echo "$input_string" | sed -E "s/.*${YEAR}-★_([0-9]+)-.*/\1/")
# number="${number:0:2}"
# number=$(( number + 1 ))
#
# new_line="[![AoC $YEAR](https://img.shields.io/badge/$YEAR-★_$number-$hex_color)](https://adventofcode.com/$YEAR)"
#
# sed -i "/$YEAR-/c\\$new_line" README.md

FOO1="yes"
# FOO2="yes"

N=2
if [ -z $FOO1 ]; then
    N=$(( N-1 ))
fi
if [ -z $FOO2 ]; then
    N=$(( N-1 ))
fi

echo $N

for (( i=0; i<$N; i++ ))
do 
   echo "Welcome $i times"
done

if [ $N -gt 0 ]; then
    echo "test"
fi
