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

invalid_id1 :: (n: int) -> bool {
    if n >= 10 && n <= 99        
        return n % 11 == 0;
    if n >= 1_000 && n <= 9_999 
        return n % 101 == 0;
    if n >= 100_000 && n <= 999_999 
        return n % 1_001 == 0;
    if n >= 10_000_000 && n <= 99_999_999
        return n % 10_001 == 0;
    if n >= 1_000_000_000 && n <= 9_999_999_999
        return n % 100_001 == 0;

    return false;
}

invalid_id2 :: (n: int) -> bool {
    if n >= 10 && n <= 99 
        return n % 11 == 0;
    if n >= 100 && n <= 999 
        return n % 111 == 0;
    if n >= 1_000 && n <= 9_999
        return n % 101 == 0 || n % 1_111 == 0;
    if n >= 10_000 && n <= 99_999
        return n % 11_111 == 0;
    if n >= 100_000 && n <= 999_999
        return n % 1_001 == 0  || n % 10_101 == 0  || n % 111_111 == 0;
    if n >= 1_000_000 && n <= 9_999_999
        return n % 1_111_111 == 0;
    if n >= 10_000_000 && n <= 99_999_999
        return n % 10_001 == 0 || n % 1_010_101 == 0 || n % 11_111_111 == 0;
    if n >= 100_000_000 && n <= 999_999_999
        return n % 1_001_001 == 0 || n % 111_111_111 == 0;
    if n >= 1_000_000_000 && n <= 9_999_999_999
        return n % 100_001 == 0 || n % 101_010_101 == 0 || n % 1_111_111_111 == 0;

    return false;
}

solve :: (lines: []string, invalid_id: (_: int) -> bool) -> int {
    ranges := parse(lines);
    defer array_free(ranges);

    count := 0;

    for range : ranges {
        for range.start_id..range.end_id {
            if invalid_id(it) count += it;
        }
    }

    return count;
}

task_one :: (lines: []string) -> int {
    return solve(lines, invalid_id1);
}


task_two :: (lines: []string) -> int {
    return solve(lines, invalid_id2);
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
