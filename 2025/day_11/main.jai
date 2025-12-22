#import "Basic";
#import "String";
#import "File";
#import "Hash_Table";

parse :: (lines: []string) -> Table(string, [..]string) {
    table: Table(string, [..]string);

    for line : lines {
        if line == "" continue;
        spl := split(line, " ");
        key := spl[0]; key = slice(key, 0, key.count - 1);

        vals: [..]string;
        for val : array_view(spl, 1) {
            array_add(*vals, val);
        }
        table_set(*table, key, vals);
    }

    empty: [..]string;
    table_set(*table, "out", empty);

    return table;
}

number_of_paths :: (table: *Table(string, [..]string), cache: *Table(string, u32), from: string, to: string) -> u32 {
    count, ok := table_find(cache, from);
    if ok {
        return count;
    } else if from == to {
        return 1;
    } else {
        local: u32 = 0;
        ns, _ := table_find(table, from);
        for ns {
            local += number_of_paths(table, cache, it, to);
        }
        table_set(cache, from, local);
        return local;
    }
}

task_one :: (lines: []string) -> int {
    table := parse(lines);
    cache: Table(string, u32);
    return number_of_paths(*table, *cache, "you", "out");
}

task_two :: (lines: []string) -> u64 {
    table := parse(lines);
    cache: Table(string, u32);

    ps := string.["svr", "fft", "dac", "out"];
    sum: u64 = 1;

    for i : 0..ps.count - 2 {
        table_reset(*cache);
        from  := ps[i];
        to    := ps[i + 1];
        local := cast(u64)number_of_paths(*table, *cache, from, to);

        sum *= local;
    }

    return sum;
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
