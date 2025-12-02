#import "Basic";
#import "String";
#import "File";
#import "Math";

Range :: struct { 
    start_id: int;
    end_id: int;
}

parse :: (lines: []string) -> []Range {
    line := lines[0];

    array := split(line, ",");
    defer array_free(array);
    ranges: [..]Range;

    for array {
        spl := split(it, "-");
        defer array_free(spl);

        range := Range.{
            string_to_int(spl[0]),
            string_to_int(spl[1])
        };

        array_add(*ranges, range);
    }

    return ranges;
}

num_is_bad :: (s: string) -> bool {
    mid := (s.count / 2);

    for idx  : 0..mid-1 {
        if s[idx] != s[mid + idx] return false;
    }
    return true;
}

bad_ranges :: (range: Range) -> int {
    count := 0;

    for range.start_id..range.end_id {
        s := tprint("%", it);
        if s.count % 2 != 0 continue;

        if num_is_bad(s) {
            count += it;
        }
    }

    return count;
}

task_one :: (lines: []string) -> int {
    ranges := parse(lines);
    defer array_free(ranges);

    count := 0;

    for range : ranges {
        count += bad_ranges(range);
    }

    return count;
}

num_is_bad2 :: (s: string, root: int) -> bool {
    for i : 1..cast(int)root {
        if s.count % i != 0 || s.count == i continue;


        pat := slice(s, 0, i);

        start := 0;
        while start < s.count {
            if slice(s, 0 + start, i) != pat  continue i;
            start += i;
        }
        return true;
    }

    return false;
}

bad_ranges2 :: (range: Range) -> int {
    count := 0;

    for range.start_id..range.end_id {
        s := tprint("%", it);
        root := cast(int)sqrt(cast(float)it);

        if num_is_bad2(s, root) {
            count += it;
        }
    }

    return count;
}

task_two :: (lines: []string) -> int {
    ranges := parse(lines);
    defer array_free(ranges);

    count := 0;

    for range : ranges {
        count += bad_ranges2(range);
    }

    return count;
}

main :: () {
    args := get_command_line_arguments();
    input := ifx args.count >= 2 then args[1] else "input";

    text, ok := read_entire_file(input);
    if !ok {
        print("Missing file %\n", input);
        exit(0);
    }
    arg := split(text, "\n");

    time(arg, .One, task_one);
    time(arg, .Two, task_two);
}


time :: (input: []string, task: Task, fn: ([]string) -> $U) {
    start := current_time_monotonic();
    res := fn(input);
    duration := operator - (current_time_monotonic(), start);
    elapsed := duration.low / 1000_000_000_000;

    if #complete task == {
        case .One;
            print("(%ms)\tTask one: \x1b[0;34;34m%\x1b[0m\n", elapsed, res);
        case .Two;
            print("(%ms)\tTask two: \x1b[0;33;10m%\x1b[0m\n", elapsed, res);
    }
}

Task :: enum {
    One;
    Two;
}
