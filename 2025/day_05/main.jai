#import "Basic";
#import "String";
#import "File";
#import "Sort";

Range :: struct {
    start: int;
    end: int;
}

range_in :: (range: Range, v: int) -> bool {
    return v >= range.start && v <= range.end;
}

parse :: (lines: []string) -> ([]Range, []int) {
    ranges: [..]Range;
    ranges.allocator = temp;
    list: [..]int;
    list.allocator = temp;

    b := true;

    for line : lines {
        if line == "" {
            b = false;
            continue;
        }

        if b {
            spl := split(line, "-");
            defer array_free(spl);

            start := string_to_int(spl[0]);
            end := string_to_int(spl[1]);

            array_add(*ranges, Range.{start, end});

        } else {
            id := string_to_int(line);
            array_add(*list, id);
        }
    }
    return ranges, list;
}

task_one :: (lines: []string) -> int {
    ranges, list := parse(lines);

    count := 0;
    for item : list {
        for range : ranges {
            if range_in(range, item) {
                count += 1;
                continue item;
            }
        }
    }
    return count;
}

task_two :: (lines: []string) -> int {
    ranges, _ := parse(lines);

    sorted := quick_sort(ranges, (r) => r.start);

    laps: [..]Range;
    laps.allocator = temp;

    for sorted {
        if laps.count == 0 {
            array_add(*laps, it);
        } else {
            if it.start > laps[laps.count - 1].end {
                array_add(*laps, it);
            }
            else if it.end > laps[laps.count - 1].end {
                laps[laps.count - 1].end = it.end;
            }
        }
    }

    count := 0;
    for laps {
        count += it.end - it.start + 1;
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
    defer array_free(arg);
    for * arg {
        if <<it == "" {
            continue;
        }
        new := trim(<<it);
        <<it = new;
    }

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
