OUTPUT=$(./main input)
OUTPUT=$(echo "$OUTPUT" | sed $'s,\x1b\\[[0-9;]*[a-zA-Z],,g')
# Regular expression for line 1
regex1="Task one:\s+(\w+)"
# Regular expression for line 2
regex2="Task two:\s+(\w+)"

# Capture value after "Task one:"
value_one=$(echo "$OUTPUT" | egrep -o "$regex1" | awk '{print $NF}')
if [ -n "$value_one" ]; then
  echo "Value after Task one: |$value_one|"
else
  echo "No match for Task one"
fi

# Capture value after "Task two:"
value_two=$(echo "$OUTPUT" | egrep -o "$regex2" | awk '{print $NF}')
if [ -n "$value_two" ]; then
  echo "Value after Task two:|$value_two|"
else
  echo "No match for Task two"
fi

