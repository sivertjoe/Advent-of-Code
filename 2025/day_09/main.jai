#import "Basic";
#import "String";
#import "File";
#import "Sort";
#import "Math";

Point :: struct { x: s64; y: s64; }
Double :: struct { a: Point; b: Point; }

Line :: struct { y: s64; xmin: s64; xmax: s64; }
VLine :: struct { x: s64; ymin: s64; ymax: s64; }

Candidate :: struct {
    y: s64;
    xmin: s64;
    xmax: s64;

    pcount: u8;
    pts: [2]Point;
}

parse :: (lines: []string) -> [..]Point {
    ret: [..]Point;
    for line : lines {
        if line == "" continue;
        spl := split(line, ",");
        array_add(*ret, Point.{ cast(s64)string_to_int(spl[0]), cast(s64)string_to_int(spl[1]) });
    }
    return ret;
}


cand_add_point :: (c: *Candidate, p: Point) {
    c.pts[c.pcount] = p;
    c.pcount += 1;
}

cand_contains :: (c: Candidate, p: Point) -> bool {
    for c.pts {
        if it.x == p.x && it.y == p.y return true;
    }
    return false;
}

cand1pt :: (y: s64, xmin: s64, xmax: s64, p: Point) -> Candidate {
    c: Candidate;
    c.y = y;
    c.xmin = xmin;
    c.xmax = xmax;
    c.pcount = 0;
    cand_add_point(*c, p);
    return c;
}

cand2pt :: (y: s64, xmin: s64, xmax: s64, p1: Point, p2: Point) -> Candidate {
    c := cand1pt(y, xmin, xmax, p1);
    cand_add_point(*c, p2);
    return c;
}
point_square :: (p1: Point, p2: Point) -> s64 {
    dx := abs(p1.x - p2.x) + 1;
    dy := abs(p1.y - p2.y) + 1;
    return dx * dy;
}

inside_excl :: (cand: Candidate, x: s64) -> bool {
    return cand.xmin < x && x < cand.xmax;
}

inside_incl :: (cand: Candidate, x: s64) -> bool {
    return cand.xmin <= x && x <= cand.xmax;
}

find_block :: (verticals: []VLine, x0: s64, y: s64, cmp: (a: s64, b: s64) -> bool) -> VLine {
    found := false;
    best: VLine;
    for verticals {
        if cmp(it.x, x0) && it.ymin <= y && y <= it.ymax {
            if !found || cmp(it.x, best.x) {
                best = it;
                found = true;
            }
        }
    }
    return best;
}

find_block_right :: (verticals: []VLine, x0: s64, y: s64) -> VLine {
    return find_block(verticals, x0, y, (a, b) => a > b);
}

find_block_left :: (verticals: []VLine, x0: s64, y: s64) -> VLine {
    return find_block(verticals, x0, y, (a, b) => a < b);
}

solve :: (verticals: []VLine, lines: [..]Line) -> s64 {
    active_candidates: [..]Candidate;
    result: s64 = 0;

    remove_flags: [..]bool;

    while lines.count > 0 {
        line := pop(*lines);

        new := cand2pt(
            line.y, line.xmin, line.xmax,
            Point.{ line.xmin, line.y },
            Point.{ line.xmax, line.y }
        );

        to_add: [..]Candidate;

        array_reserve(*remove_flags, active_candidates.count);
        remove_flags.count = 0;
        for 0..active_candidates.count - 1 array_add(*remove_flags, false);

        has_extended_left := false;
        for cand : active_candidates {
            if cand.xmin == new.xmax {
                has_extended_left = true;

                if cand_contains(cand, Point.{ cand.xmax, cand.y }) {
                    a := point_square(Point.{ new.xmax, new.y }, Point.{ cand.xmax, cand.y });
                    if a > result result = a;
                }
            }
        }
        if has_extended_left {
            block_right := find_block_right(verticals, new.xmax, new.y);
            c := cand1pt(new.y, new.xmin, block_right.x, Point.{ new.xmin, new.y });
            array_add(*to_add, c);
        }

        has_extended_right := false;
        for cand : active_candidates {
            if cand.xmax == new.xmin {
                has_extended_right = true;

                if cand_contains(cand, Point.{ cand.xmin, cand.y }) {
                    a := point_square(Point.{ new.xmin, new.y }, Point.{ cand.xmin, cand.y });
                    if a > result result = a;
                }
            }
        }
        if has_extended_right {
            block_left := find_block_left(verticals, new.xmin, new.y);
            c := cand1pt(new.y, block_left.x, new.xmax, Point.{ new.xmax, new.y });
            array_add(*to_add, c);
        }

        for active_candidates {
            if new.xmin <= it.xmin && it.xmax <= new.xmax {
                remove_flags[it_index] = true;
            }
        }

        overlapped_any := false;

        for cand : active_candidates {
            if inside_excl(cand, new.xmin) || inside_excl(cand, new.xmax) {
                overlapped_any = true;

                for pi : 0..cast(s64)cand.pcount - 1 {
                    p := cand.pts[pi];

                    if inside_incl(cand, new.xmin) {
                        a := point_square(p, Point.{ new.xmin, new.y });
                        if a > result result = a;
                    }
                    if inside_incl(cand, new.xmax) {
                        a := point_square(p, Point.{ new.xmax, new.y });
                        if a > result result = a;
                    }
                }

                if inside_excl(cand, new.xmin) && cand_contains(cand, Point.{ cand.xmin, cand.y }) {
                    c1 := cand1pt(cand.y, cand.xmin, new.xmin, Point.{ cand.xmin, cand.y });
                    array_add(*to_add, c1);

                    c2 := cand1pt(new.y, cand.xmin, new.xmin, Point.{ new.xmin, new.y });
                    array_add(*to_add, c2);
                }

                if inside_excl(cand, new.xmax) && cand_contains(cand, Point.{ cand.xmax, cand.y }) {
                    c1 := cand1pt(cand.y, new.xmax, cand.xmax, Point.{ cand.xmax, cand.y });
                    array_add(*to_add, c1);

                    c2 := cand1pt(new.y, new.xmax, cand.xmax, Point.{ new.xmax, new.y });
                    array_add(*to_add, c2);
                }

                remove_flags[it_index] = true;
            }
        }

        if !overlapped_any array_add(*to_add, new);

        for #v2 < idx: 0..active_candidates.count-1 {
            if remove_flags[idx] {
                array_unordered_remove_by_index(*active_candidates, idx);
            }
        }

        for to_add array_add(*active_candidates, it);
    }

    return result;
}

task_one :: (lines: []string) -> s64 {
    points := parse(lines);
    m: s64 = 0;

    for i : 0..points.count - 1 {
        for j : i + 1..points.count - 1 {
            m = max(m, point_square(points[i], points[j]));
        }
    }
    return m;
}

task_two :: (input: []string) -> s64 {
    reds := parse(input);

    n := reds.count;
    lines: [..]Double;
    for i : 0..reds.count - 1 {
        a := reds[i];
        b := reds[(i + 1) % n];
        array_add(*lines, Double.{ a, b });
    }

    horizontals: [..]Line;
    for seg : lines {
        if seg.a.y != seg.b.y continue;
        array_add(*horizontals, Line.{ seg.a.y, min(seg.a.x, seg.b.x), max(seg.a.x, seg.b.x) });
    }
    quick_sort(horizontals, line => line.y);

    verticals: [..]VLine;
    for seg : lines {
        if seg.a.x != seg.b.x continue;
        array_add(*verticals, VLine.{ seg.a.x, min(seg.a.y, seg.b.y), max(seg.a.y, seg.b.y) });
    }
    quick_sort(verticals, v => v.x);

    return solve(verticals, horizontals);
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
