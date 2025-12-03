#import "Basic";
#import "String";
#import "File";

solve :: (lines: []string, N: int) ->  u64 {
    sum: u64 = 0;

    a: [..]int;
    array_free(a);
    for 0..N-1 array_add(*a, it);


    for line : lines {
        if line == "" continue;

        for num : line {
            for k : 0..N-1 {
                if num > line[a[k]] && (line.count - it_index) > (N - k - 1) && (k == 0 || it_index > a[k-1]) {
                    for j : k..N-1 {
                        a[j] = it_index + j - k;
                    }
                    break;
                }
            }
        }

        z := #char "0";
        local: u64 = 0;
        for elem : a {
            local = local * 10 + (cast(u64)(line[elem] - z));
        }
        sum += local;
    }

    return sum;
}

task_one :: (lines: []string) -> u64 {
    return solve(lines, 2);
}

task_two :: (lines: []string) -> u64 {
    return solve(lines, 12);
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
