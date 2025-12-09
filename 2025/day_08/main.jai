#import "Basic";
#import "String";
#import "File";
#import "Math";
#import "Sort";

Point :: struct {
    x: u64;
    y: u64;
    z: u64;
}

fold64 :: (v: u64) -> u32 {
    lo := v & 0xFFFFFFFF;
    hi := (v >> 32) & 0xFFFFFFFF;
    return cast(u32)(lo ^ hi);
}


point_hash :: (p: Point) -> u32 {
    h: u64;
    h = p.x * 11400714819323198485;
    h ^= p.y * 14029467366897019727;
    h ^= p.z *  1609587929392839161;
    return fold64(h);
}


point_eq :: (a: Point, b: Point) -> bool { return a.x == b.x && a.y == b.y && a.z == b.z; }

point_dist :: (a: Point, b: Point) -> u64 {
    dst :: (a: u64, b: u64) -> u64 {
        d := a - b;
        return d * d;
    }
    return dst(a.x, b.x) + dst(a.y, b.y) + dst(a.z, b.z);
}

parse :: (lines: []string) -> [..]Point {
    ret: [..]Point;

    for lines {
        if it == "" continue;

        spl := split(it, ",");
        p := Point.{
            cast(u64)string_to_int(spl[0]),
            cast(u64)string_to_int(spl[1]),
            cast(u64)string_to_int(spl[2]),
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




find :: (parent: []int, x: int) -> int {
    if parent[x] != x {
        parent[x] = find(parent, parent[x]);
    }
    return parent[x];
}

union_sets :: (parent: []int, size: []int, a: int, b: int) {
    ra := find(parent, a);
    rb := find(parent, b);

    if ra == rb return;

    if size[ra] < size[rb] {
        ra, rb = rb, ra;
    }

    parent[rb] = ra;
    size[ra]  += size[rb];
}

solve :: (lines: []string, part_1: bool) -> u64 {
    N :: 1000;
    points := parse(lines);

    dists := calc_dists(points);
    quick_sort(dists, (p) => p.z);

    parent: [..]int;
    size: [..]int;
    array_resize(*parent, points.count);
    array_resize(*size, points.count);

    for i : 0..points.count-1 {
        parent[i] = i;
        size[i] = 1;
    }

    for dists {
        if it_index == N-1 && part_1 break;
        ra := find(parent, xx it.x);
        rb := find(parent, xx it.y);

        if ra != rb {
            union_sets(parent, size, ra, rb);
        }

        root := find(parent, 0);

        if size[root] == points.count {
            a := points[it.x];
            b := points[it.y];
            return a.x * b.x;
        }
    }

    groups: [..]u64;
    for i : 0..points.count-1 {
        if parent[i] == i {
            array_add(*groups, cast(u64)size[i]);
        }
    }

    quick_sort(groups, x => x);

    return groups[groups.count-1]
         * groups[groups.count-2]
         * groups[groups.count-3];
}

task_one :: (lines: []string) -> u64 {
    return solve(lines, true);
}

task_two :: (lines: []string) -> u64 {
    return solve(lines, false);
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
