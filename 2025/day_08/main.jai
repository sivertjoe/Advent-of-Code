#import "Basic";
#import "String";
#import "File";
#import "Math";
#import "Sort";
#load "../template/set.jai";

Point :: struct {
    y: u32;
    x: u32;
    z: u32;
}

point_hash :: (p: Point) -> u32 {
    hash: u32 = 2166136261;
    prime: u32 = 16777619;

    return (((((hash ^ p.x) * prime) ^ p.y) * prime) ^ p.z) * prime;
}

point_eq :: (a: Point, b: Point) -> bool { return a.x == b.x && a.y == b.y && a.z == b.z; }

point_dist :: (a: Point, b: Point) -> int {
    dst :: (a: u32, b: u32) -> int {
        return xx pow((cast(float) a - cast(float) b), 2);
    }
    return dst(a.x, b.x) + dst(a.y, b.y) + dst(a.z, b.z);
}

parse :: (lines: []string) -> [..]Point {
    ret: [..]Point;

    for lines {
        if it == "" continue;

        spl := split(it, ",");
        p := Point.{
            cast(u32)string_to_int(spl[0]),
            cast(u32)string_to_int(spl[1]),
            cast(u32)string_to_int(spl[2]),
        };
        array_add(*ret, p);
    }

    return ret;
}

calc_dists :: (points: []Point) -> [..]Point {
    dists: [..]Point;
    for i : 0..points.count - 1 {
        for j : i + 1..points.count - 1 {
            dist := point_dist(points[i], points[j]);
            key: Point;
            key.x = xx i;
            key.y = xx j;
            key.z = xx dist;
            array_add(*dists, key);
        }
    }

    return dists;
}
task_one :: (lines: []string) -> int {
    points := parse(lines);

    dists := calc_dists(points);
    sorted := quick_sort(dists, (p) => p.z);

    circuts: [..]Set(Point, point_hash, point_eq);
    for i: 1..10 {
        dist := dists[i];
        a := points[dist.x];
        b := points[dist.y];

        ia := -1;
        ib := -1;

        for * circuts {
            if set_contains(it, a) {
                ia = it_index;
            }
            if set_contains(it, b) {
                ib = it_index;
            }
        }

        if ia != -1 && ib != -1 {

             Type wanted: *Set(Value_Type=Point, given_hash_function=null, given_cmp_function=null);
             type given: *Set(Value_Type=Point, given_hash_function=point_hash, given_cmp_function=point_eq).
            set := set_union(*circuts[ia], *circuts[ib]);
            array_unordered_remove_by_index(*circuts, ia);
        }

    }

    return 0;
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
