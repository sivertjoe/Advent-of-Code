#import "Basic";
#import "String";
#import "File";

task_one :: (lines: []string) -> int {
    z := #char "0";

    sum := 0;
    for line : lines {
        if line == "" continue;
        left: int = -1;
        right: int = -1;
        for ch : line {
            num := cast(int)(ch - z);
            if num > left && it_index != line.count - 1 {
                left = num;
                right = line[it_index+1] - z;
            } else if num > right {
                right = num;
            }
        }
        sum += left * 10 + right;
    }
    return sum;
}

task_two :: (lines: []string) -> u64 {
    z := #char "0";

    N :: 11;
    sum :u64= 0;
    for line : lines {
        if line == "" continue;
        count := line.count;
        a1 := 0;
        a2 := 1;
        a3 := 2;
        a4 := 3;
        a5 := 4;
        a6 := 5;
        a7 := 6;
        a8 := 7;
        a9 := 8;
        a10 := 9;
        a11 := 10;
        a12 := 11;
            //if num > a1 && (count - it_index) >= (N  + 0) {

        it_index := 0;
        while it_index < count {
            num := line[it_index];
            if num > line[a1] && (count - it_index) > (N - 0) {
                a1 = it_index;
                a2 = it_index + 1;
                a3 = it_index + 2;
                a4 = it_index + 3;
                a5 = it_index + 4;
                a6 = it_index + 5;
                a7 = it_index + 6;
                a8 = it_index + 7;
                a9 = it_index + 8;
                a10 = it_index + 9;
                a11 = it_index + 10;
                a12 = it_index + 11;
            } else if num > line[a2] && (count - it_index) > (N - 1) && it_index > a1 {
                a2 = it_index;
                a3 = it_index + 1;
                a4 = it_index + 2;
                a5 = it_index + 3;
                a6 = it_index + 4;
                a7 = it_index + 5;
                a8 = it_index + 6;
                a9 = it_index + 7;
                a10 = it_index + 8;
                a11 = it_index + 9;
                a12 = it_index + 10;
            }
            else if num > line[a3] && (count - it_index) > (N - 2) && it_index > a2 {
                a3 = it_index;
                a4 = it_index + 1;
                a5 = it_index + 2;
                a6 = it_index + 3;
                a7 = it_index + 4;
                a8 = it_index + 5;
                a9 = it_index + 6;
                a10 = it_index + 7;
                a11 = it_index + 8;
                a12 = it_index + 9;
            }
            else if num > line[a4] && (count - it_index) > (N - 3) && it_index > a3 {
                a4 = it_index;
                a5 = it_index + 1;
                a6 = it_index + 2;
                a7 = it_index + 3;
                a8 = it_index + 4;
                a9 = it_index + 5;
                a10 = it_index + 6;
                a11 = it_index + 7;
                a12 = it_index + 8;
            }
            else if num > line[a5] && (count - it_index) > (N - 4) && it_index > a4 {
                a5 = it_index + 0;
                a6 = it_index + 1;
                a7 = it_index + 2;
                a8 = it_index + 3;
                a9 = it_index + 4;
                a10 = it_index + 5;
                a11 = it_index + 6;
                a12 = it_index + 7;
            }
            else if num > line[a6] && (count - it_index) > (N - 5) && it_index > a5 {
                a6 = it_index + 0;
                a7 = it_index + 1;
                a8 = it_index + 2;
                a9 = it_index + 3;
                a10 = it_index + 4;
                a11 = it_index + 5;
                a12 = it_index + 6;
            }
            else if num > line[a7] && (count - it_index) > (N - 6) && it_index > a6 {
                a7 = it_index + 0;
                a8 = it_index + 1;
                a9 = it_index + 2;
                a10 = it_index + 3;
                a11 = it_index + 4;
                a12 = it_index + 5;
            }
            else if num > line[a8] && (count - it_index) > (N - 7) && it_index > a7 {
                a8 = it_index + 0;
                a9 = it_index + 1;
                a10 = it_index + 2;
                a11 = it_index + 3;
                a12 = it_index + 4;
            }
            else if num > line[a9] && (count - it_index) > (N - 8) && it_index > a8 {
                a9 = it_index + 0;
                a10 = it_index + 1;
                a11 = it_index + 2;
                a12 = it_index + 3;
            }
            else if num > line[a10] && (count - it_index) > (N - 9) && it_index > a9 {
                a10 = it_index + 0;
                a11 = it_index + 1;
                a12 = it_index + 2;
            }
            else if num > line[a11] && (count - it_index) > (N - 10) && it_index > a10 {
                a11 = it_index + 0;
                a12 = it_index + 1;
            }
            else if num > line[a12] && (count - it_index) > (N - 11) && it_index > a11 {
                a12 = it_index + 0;
            }
            it_index += 1;
        }

        local :=
              (cast(u64)(line[a12]-z))
            +(cast(u64)(line[a11]-z)) * 10
            +(cast(u64)(line[a10]-z)) * 100
            +(cast(u64)(line[a9]-z)) *  1000
            +(cast(u64)(line[a8]-z)) *  10000
            +(cast(u64)(line[a7]-z)) *  100000
            +(cast(u64)(line[a6]-z)) *  1000000
            +(cast(u64)(line[a5]-z)) *  10000000
            +(cast(u64)(line[a4]-z)) *  100000000
            +(cast(u64)(line[a3]-z)) *  1000000000
            +(cast(u64)(line[a2]-z)) *  10000000000
            +(cast(u64)(line[a1]-z)) *  100000000000;
        sum += local;
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
