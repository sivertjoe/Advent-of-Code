#import "Basic";
#import "String";
#import "File";
#import "Math";

mod :: (a: int, n: int) -> int {
    return ((a % n) + n) % n;
}

parse_line :: (line: string) -> int {
    builder: String_Builder;
    init_string_builder(*builder);
    for line {
        if it_index == 0 continue;
        append(*builder, it);
    }
    s := builder_to_string(*builder);
    num := string_to_int(s);

    return ifx line[0] == #char "L" then -num else num;
}

solve :: (lines: []string, N := 100) -> part1: int, part2: int {
    p1, p2: int;
    dial : = 50;
    for lines {
        if it == "" continue;
        num := parse_line(it);
        raw := dial + num;
        p1 +=  xx !(raw % N);
        p2 += (abs(raw) / N) + xx (dial != 0 && raw <= 0);
        dial = mod(raw, N);
    }

    return p1, p2;
}

task_one :: (lines: []string) -> int {
    p1, _ := solve(lines);
    return p1;
}

task_two :: (lines: []string) -> int {
    _, p2 := solve(lines);
    return p2;
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
