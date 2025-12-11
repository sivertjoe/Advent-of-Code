#import "Basic";
#import "String";
#import "File";
#import "Z3";

Info :: struct {
    lights: [..]int;
    wires: [..][..]int;
    jolts: [..]int;
}

parse_line :: (line: string) -> Info {
    parse_lights :: (s: string) -> [..]int {
        ls := slice(s, 1, s.count - 2);
        lights: [..]int;

        for ls {
            n := ifx it == #char "." then 0 else 1;
            array_add(*lights, n);
        }
        return lights;
    }
    parse_wires :: (ws: []string) -> [..][..]int {
        w: [..][..]int;

        for ws {
            s := slice(it, 1, it.count - 2);
            spl := split(s, ",");
            arr: [..]int;
            for n : spl {
                array_add(*arr, string_to_int(n));
            }
            array_add(*w, arr);
        }
        return w;
    };

    parse_jolts :: (s: string) -> [..]int {
        t: [..]int;
        spl := split(slice(s, 1, s.count - 2), ",");
        for spl array_add(*t, string_to_int(it));
        return t;
    }

    spl := split(line, " ");

    lights := parse_lights(spl[0]);
    wires := parse_wires(array_view(spl, 1, spl.count-2));
    jolts := parse_jolts(spl[spl.count - 1]);

    info := Info.{lights, wires, jolts};
    return info;
}

parse :: (lines: []string) -> [..]Info {
    infos: [..]Info;
    for lines {
        if it == "" continue;
        array_add(*infos, parse_line(it));
    }
    return infos;
}

solve :: (cols: [..][..]int, target: []int) -> int{
    print("test\n");
    cfg := Z3_mk_confi();
    //
    // m := 0;
    // for col : cols for col m = max(m, it);

    return 0;
}

task_one :: (lines: []string) -> int {
    infos := parse(lines);
    return solve(infos[0].wires, infos[0].jolts);
}

task_two :: (lines: []string) -> int {
    return 0;
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
