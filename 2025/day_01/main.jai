#import "Basic";
#import "String";
#import "File";

task_one :: (lines: []string) -> int {
    N :: 100;
    count := 0;
    start := 50;
    for line : lines {
        if line == "" continue;
        builder: String_Builder;
        init_string_builder(*builder);

        for ch : line {
            if it_index == 0 continue;
            append(*builder, ch);
        }
        s := builder_to_string(*builder);
        num := string_to_int(s);

        if line[0] == 76 {
            start = ((start + num) % N + N) % N;
        } else {
            start = ((start - num) % N + N) % N;
        }
        if start == 0 {
            count += 1;
        }
    }

    return count;
}

task_two :: (lines: []string) -> int {
    N :: 100;
    count := 0;
    start := 50;
    for line : lines {
        if line == "" continue;
        builder: String_Builder;
        init_string_builder(*builder);

        for ch : line {
            if it_index == 0 continue;
            append(*builder, ch);
        }
        s := builder_to_string(*builder);
        num := string_to_int(s);

        if line[0] == 76 {
            for 1..num {
                start = ((start - 1) % N + N) % N;
                if start == 0 {
                    count += 1;
                }
            }
        } else {
            for 1..num {
                start = ((start + 1) % N + N) % N;
                if start == 0 {
                    count += 1;
                }
            }
        }
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
