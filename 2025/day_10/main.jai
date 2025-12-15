#import "Basic";
#import "String";
#import "File";
#import "Hash_Table";
#import "Z3";
#import "Thread";

Info :: struct {
    lights: [..]bool;
    wires: [..][..]int;
    jolts: [..]int;
}

parse_lights :: (s: string) -> [..]bool {
    ls := slice(s, 1, s.count - 2);
    lights: [..]bool;

    for ls {
        n := ifx it == #char "." then false else true;
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

parse_line :: (line: string) -> Info {
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

State :: struct {
    steps: int;
    mask: u64;
}

bfs :: (lights: []bool, wiring: [..][..]int) -> int {
    target_mask: u64 = 0;
    for lights {
        if it {
            target_mask |= cast(u64)1 << cast(u64)it_index;
        }
    }

    wiring_masks: [..]u64;
    for wiring {
        m: u64 = 0;
        for it {
            m |= cast(u64)1 << cast(u64)it;
        }
        array_add(*wiring_masks, m);
    }

    visited: Table(u64, bool);
    queue: [..]u64;

    initial_mask: u64 = 0;
    array_add(*queue, initial_mask);
    table_set(*visited, initial_mask, true);

    head := 0;
    steps := 0;

    while head < queue.count {
        layer_end := queue.count;

        while head < layer_end {
            curr := queue[head];
            head += 1;

            if curr == target_mask {
                return steps;
            }

            for wiring_masks {
                next := curr ^ it;
                if !table_contains(*visited, next) {
                    table_set(*visited, next, true);
                    array_add(*queue, next);
                }
            }
        }

        steps += 1;
    }

    return -1;
}

solve :: (info: Info) -> int {
    cols := info.wires;
    target := info.jolts;
    ctx := Context_Create();
    defer Context_Destroy(ctx);

    opt := Optimize_Create(ctx);
    defer Optimize_Destroy(opt);

    xs := NewArray(cols.count, Int);
    defer array_free(xs);
    for cols {
        name := tprint("x%", it_index);
        xs[it_index] = Int_Create(name, ctx);
    }

    for r : 0..target.count - 1 {
        terms: [..]Z3_ast;

        for c : 0..cols.count - 1 {
            for row_idx : cols[c] {
                if row_idx == r {
                    array_add(*terms, xs[c]);
                    break;
                }
            }
        }

        sum := Int_Add(terms, ctx);
        target_val := Int_Create(target[r], ctx);
        eq := Int_Eq(sum, target_val, ctx);
        Optimize_Add(opt, eq);
    }

    // Non-negative
    for xs {
        ge := Int_Ge(it, Int_Create(0, ctx), ctx);
        Optimize_Add(opt, ge);
    }

    total_sum := Int_Add(xs, ctx);
    Optimize_Minimize(opt, total_sum);

    assert(Optimize_Check(opt));
    model := Optimize_GetModel(opt);
    defer Model_Destroy(model);
    Z3_model_inc_ref(ctx, model);

    sum := 0;
    for xs {
        ok, val := Model_EvalInt(model, it);
        if ok {
            sum += val;
        }
    }

    return sum;
}

Work :: struct {
    info: Info;
    result: int;
}
thread_task :: (group: *Thread_Group, thread: *Thread, work: *void) -> Thread_Continue_Status {
    w := cast(*Work)work;
    w.result = solve(w.info);
    return .CONTINUE;
}

task_one :: (lines: []string) -> int {
    infos := parse(lines);

    sum := 0;
    for infos {
        sum += bfs(it.lights, it.wires);
    }
    return sum;
}

task_two :: (lines: []string) -> int {
    infos := parse(lines);

    tg: Thread_Group;
    init(*tg, 8, thread_task, true);
    tg.logging = false;

    start(*tg);
    work:[..]Work;
    for infos {
        w := Work.{it, 0};
        array_add(*work, w);
    }
    for infos {
        add_work(*tg, *work[it_index]);
    }

    work_remaining := infos.count;
    sum := 0;
    while work_remaining > 0 {
        list := get_completed_work(*tg);
        for list {
            val := cast(*Work)it;
            sum += val.result;
        }
        work_remaining -= list.count;
    }

    shutdown(*tg);
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
