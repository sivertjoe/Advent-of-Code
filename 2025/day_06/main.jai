#import "Basic";
#import "String";
#import "File";

Op :: struct {
    acc: int;
    func: (x: int, y: int) -> int;
}

get_ops :: (lines: []string) -> [..]Op {
    add :: (a: int, b: int)  -> int { return a + b; };
    prod :: (a: int, b: int) -> int { return a * b; };
    ops: [..]Op;

    for 0..lines[0].count - 1 {
        ch := lines[lines.count - 1][it];

        if ch == {
            case #char "*";
                array_add(*ops, Op.{1, prod});
            case #char "+";
                array_add(*ops, Op.{0, add});
        }
    }

    return ops;
}

parse1 :: (lines: []string) -> [..][..]int {
    temp: [..][..]int;

    for 0..lines.count - 2 {
        line := lines[it];
        arr: [..]int;
        for val : split(line, " ") {
            if val != "" array_add(*arr, string_to_int(val));
        }
        array_add(*temp, arr);
    }

    transpose :: (mat: [..][..]int) -> [..][..]int {
        new: [..][..]int;
        for y : 0..mat[0].count - 1 {
            arr: [..]int;
            for x : 0..mat.count - 1 {
                array_add(*arr, mat[x][y]);
            }
            array_add(*new, arr);
        }
        return new;
    }

    return transpose(temp);
}

parse2 :: (lines: []string) -> [..][..]int {
    z: u8 = #char "0";

    list: [..][..]int;
    for x : 0..lines[0].count -1 {
        num := 0;
        pow := 1;
        for yy : 0..lines.count -1 {
            y := lines.count - 1 - yy;
            ch := lines[y][x];

            if ch == #char " " continue;

            if ch == #char "*" || ch == #char "+" {
                lst: [..]int;
                array_add(*list, lst);
                continue;
            } 

            num = num + (ch - z) * pow;
            pow *= 10;
        }
        if num != 0 array_add(*list[list.count -1], num);
    }

    return list;
}

solve :: (lines: []string, parse: (_: []string) -> ([..][..]int)) -> int {
    ops := get_ops(lines);
    nums := parse(lines);
    count := 0;
    for * ops {
        for num : nums[it_index] {
            it.acc = it.func(it.acc, num);
        }
        count += it.acc;
    }
    return count;
}

task_one :: (lines: []string) -> int {
    return solve(lines, parse1);
}

task_two :: (lines: []string) -> int {
    return solve(lines, parse2);
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
        if <<it == "" remove it;
    }

    context.allocator = temp;
    time(arg, .One, task_one);
    reset_temporary_storage();
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
