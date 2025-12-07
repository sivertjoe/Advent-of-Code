#import "Basic";
#import "String";
#import "File";
#import "Hash_Table";

Cell :: enum u8 {
    Empty :: 5;
    Beam;
    Split;
}

parse :: (lines: []string) -> ([..][..]Cell, x: int) {
    ret: [..][..]Cell;
    sx: int;

    for line, y: lines {
        row: [..]Cell;
        for ch, x : line {
            if line == "" continue;
            if ch == {
                case #char "S";
                    sx = x;
                    array_add(*row, Cell.Empty);
                case #char ".";
                    array_add(*row, Cell.Empty);
                case #char "^";
                    array_add(*row, Cell.Split);
            }
        }
        if row.count > 0 {
            array_add(*ret, row);
        }
    }
    return ret, sx;
}

Point :: struct {
    y: u32;
    x: u32;
}

point_hash :: (p: Point) -> u32 {
    hash: u32 = 2166136261;
    prime: u32 = 16777619;

    return (((hash ^ p.x) * prime) ^ p.y) * prime;
}

point_eq :: (a: Point, b: Point) -> bool { return a.x == b.x && a.y == b.y; }

count_timelines :: (
    map: [..][..]Cell, 
    p: Point,
    cache: *Table(Point, int, point_hash, point_eq)) -> int {

    h := map.count;
    w := map[0].count;

    if p.y == h {
        return 1;
    }

    val, ok := table_find(cache, p);
    if ok return val;

    sum := 0;
    if map[p.y][p.x] == Cell.Split {
        left := count_timelines(map, Point.{p.y + 1, p.x - 1}, cache);
        right := count_timelines(map, Point.{p.y + 1, p.x + 1}, cache);
        sum = left + right;
    } else {
        sum = count_timelines(map, Point.{p.y + 1, p.x}, cache);
    }

    table_add(cache, p, sum);
    return sum;

}

task_one :: (lines: []string) -> int {
    map, sx := parse(lines);

    count := 0;
    map[1][sx] = Cell.Beam;
    for y : 1..map.count - 2 {
        for x : 0..map[0].count - 1 {
            if map[y][x] == Cell.Beam {
                if map[y + 1][x] == {
                    case Cell.Empty;
                        map[y + 1][x] = Cell.Beam;
                    case Cell.Beam;
                        map[y + 1][x] = Cell.Beam;
                    case Cell.Split;
                        map[y + 1][x + 1] = Cell.Beam;
                        map[y + 1][x - 1] = Cell.Beam;
                        count += 1;
                }
            }
        }
    }
    return count;
}


task_two :: (lines: []string) -> int {
    map, sx := parse(lines);
    cache: Table(Point, int, point_hash, point_eq);
    return count_timelines(map, Point.{0, xx sx}, *cache);
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
