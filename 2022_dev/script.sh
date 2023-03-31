YEAR=$1
DAY=$2
LEVEL=$3
ANSWER=$4
COOKIE="session=53616c7465645f5f44763025a95f1c1dc59510367cca79cf79f316cdde6e599548248373f8e02962a93f2f6c228fb234a5e2067b065bce461c7381eb956fc74e"

STR=$(curl -X POST https://adventofcode.com/2022/day/$DAY/answer -H "Content-Type: application/x-www-form-urlencoded" -d "level=$LEVEL&answer=$ANSWER" --cookie "session=53616c7465645f5f44763025a95f1c1dc59510367cca79cf79f316cdde6e599548248373f8e02962a93f2f6c228fb234a5e2067b065bce461c7381eb956fc74e")
#
SUB="That's the right answer"
if [[ "$STR" == *"$SUB"* ]]; then
    echo "Its there"
fi

#URL=https://adventofcode.com/$YEAR/day/$DAY/input 
#echo $URL
#curl $URL --cookie $COOKIE > input

# string1="day_02"
# string2="day_15"
#
# # Extract the numeric part of the strings
# num1=$(echo "${string1#day_}" | sed 's/^0*//')
# num2=$(echo "${string2#day_}" | sed 's/^0*//')
#
# # Print the numeric parts
# echo "num1: $num1"
# echo "num2: $num2"
#
#!/bin/bash

# Set the input string and regex
