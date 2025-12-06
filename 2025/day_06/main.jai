#import "Basic";
#import "String";
#import "File";

parse :: (lines: []string) -> [..][..]string {
    ret: [..][..]string;

    for lines {
        s := trim(it);
        if s == "" continue;
        arr:[..]string;

        for val : split(s, " ") {
            if val != "" {
                array_add(*arr, val);
            }
        }
        array_add(*ret, arr);
    }
    return ret;
}

get :: (op: string) -> (int, (x: int, y: int) -> int) {
    add :: (a: int, b: int)  -> int { return a + b; };
    prod :: (a: int, b: int) -> int { return a * b; };

    if op == "*" 
        return 1, prod;
    else if op == "+"
        return 0, add;

    assert(false);
}

task_one :: (lines: []string) -> int {
    sheet := parse(lines);

    count := 0;
    for op_idx : 0..sheet[0].count - 1 {
        op := sheet[sheet.count - 1][op_idx];

        acc, func := get(op);
        for num_idx : 0..sheet.count - 2 {
            num := string_to_int(sheet[num_idx][op_idx]);
            acc = func(acc, num);
        }
        count += acc;
    }
    return count;
}

Op :: struct {
    acc: int;
    func: (x: int, y: int) -> int;
}
get2 :: (op: u8) -> Op {
    add :: (a: int, b: int)  -> int { return a + b; };
    prod :: (a: int, b: int) -> int { return a * b; };

    if op == #char "*" 
        return Op.{1, prod};
    else if op == #char "+"
        return Op.{0, add};

    assert(false);
}

task_two :: (lines: []string) -> int {
    z: u8 = #char "0";

    list: [..][..]int;
    opers: [..]Op;

    for x : 0..lines[0].count -1 {
        num: int;
        pow := 1;
        for yy : 0..lines.count -1 {
            y := lines.count - 1 - yy;
            ch := lines[y][x];

            if ch == #char "*" || ch == #char "+" {
                array_add(*opers, get2(ch));
                lst: [..]int;
                array_add(*list, lst);
            } else {
                if ch != #char " " {
                    num = num + (ch - z) * pow;
                    pow *= 10;
                }
            }
        }
        if num != 0 {
            array_add(*list[list.count -1], num);
        }
    }

    assert(opers.count == list.count);
    count := 0;
    for 0..opers.count - 1 {
        op := opers[it];

        for num : list[it] {
            op.acc = op.func(op.acc, num);
        }
        count += op.acc;
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
    }
    arg.count -= 1;

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
