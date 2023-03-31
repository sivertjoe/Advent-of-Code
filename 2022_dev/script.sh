YEAR=$1
DAY=$2
LEVEL=$3
ANSWER=$4
STR=$(curl -X POST https://adventofcode.com/2022/day/$DAY/answer -H "Content-Type: application/x-www-form-urlencoded" -d "level=$LEVEL&answer=$ANSWER" --cookie "session=53616c7465645f5f44763025a95f1c1dc59510367cca79cf79f316cdde6e599548248373f8e02962a93f2f6c228fb234a5e2067b065bce461c7381eb956fc74e")

SUB="That's the right answer"
if [[ "$STR" == *"$SUB"* ]]; then
    echo "Its there"
fi
