STR=$(git diff --name-only HEAD^ HEAD)

year=$(echo "$STR" | grep -oE "[0-9]{4}" | head -n 1)

day=$(echo "$STR" | grep -oE "day_[0-9]{2}" | head -n 1)

if [ -z "$year" ] || [ -z "$day" ]; then
    exit 1
fi

# printf "$year $day"
read yy ded <<< "$year $day"
echo " asd $yy $ded"

