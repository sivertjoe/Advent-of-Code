#import "Basic";
#import "String";
#import "File";

Point :: struct {
    x: s64;
    y: s64;
}

Segment :: struct {
    a: Point;
    b: Point;
}

parse :: (lines: []string) -> [..]Point {
    ret: [..]Point;

    for lines {
        if it == "" continue;
        spl := split(it, ",");
        p := Point.{
            cast(s64)string_to_int(spl[0]),
            cast(s64)string_to_int(spl[1])
        };
        array_add(*ret, p);
    }
    return ret;
}

point_square :: (a: Point, b: Point) -> s64 {
    min_x := min(a.x, b.x);
    max_x := max(a.x, b.x);

    min_y := min(a.y, b.y);
    max_y := max(a.y, b.y);
    return (max_x - min_x + 1) * (max_y - min_y + 1);
}

task_one :: (lines: []string) -> s64 {
    points := parse(lines);
    m := 0;

    for i : 0..points.count - 1 {
        for j : i + 1..points.count - 1{
            m = max(m, point_square(points[i], points[j]));
        }
    }
    return m;
}

orient :: (a: Point, b: Point, c: Point) -> int {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

on_segment :: (a: Point, b: Point, p: Point) -> bool {
    if orient(a, b, p) != 0 return false;

    return p.x >= min(a.x, b.x) 
        && p.x <= max(a.x, b.x)
        && p.y >= min(a.y, b.y)
        && p.y <= max(a.y, b.y);
}

segments_cross_properly :: (a: Point, b: Point, c: Point, d: Point) -> bool {
    o1 := orient(a, b, c);
    o2 := orient(a, b, d);
    o3 := orient(c, d, a);
    o4 := orient(c, d, b);

    ab_straddles_cd := (o1 < 0 && o2 > 0) || (o1 > 0 && o2 < 0);
    cd_straddles_ab := (o3 < 0 && o4 > 0) || (o3 > 0 && o4 < 0);

    return ab_straddles_cd && cd_straddles_ab;
}

polygon_edges :: (vertices: []Point) -> [..]Segment {
    n := vertices.count;
    edges: [..]Segment;
    for i : 0..n-1 {
        a := vertices[i];
        b := vertices[(i + 1) % n];
        array_add(*edges, Segment.{a, b});
    }
    return edges;
}

point_on_polygon_edge :: (p: Point, vertices: []Point) -> bool {
    edges := polygon_edges(vertices);

    for edges {
        if on_segment(it.a, it.b, p) {
            return true;
        }
    }
    return false;
}

point_in_or_on_polygon :: (p: Point, vertices: []Point) -> bool {
    if point_on_polygon_edge(p, vertices) {
        return true;
    }

    px, py := (cast(float64)p.x, cast(float64)p.y);

    inside := false;
    n := vertices.count;
    for i : 0..n - 1 {
        x1, y1 := cast(float64)vertices[i].x, cast(float64)vertices[i].y;
        x2, y2 := 
            cast(float64)vertices[(i + 1) % n].x,
            cast(float64)vertices[(i + 1) % n].y;

            intersects := ((y1 > py) != (y2 > py)) && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1);

        if intersects {
            inside = !inside;
        }
    }
    return inside;
}

move_is_allowed :: (old_p: Point, new_p: Point, polygon: []Point, edges: []Segment) -> bool {
    if !point_in_or_on_polygon(old_p, polygon) {
        return false;
    }
    if !point_in_or_on_polygon(new_p, polygon) {
        return false;
    }

    for edges {
        if segments_cross_properly(old_p, new_p, it.a, it.b) {
            return false;
        }
    }

    return true;
}

is_points_all_inside :: (a: Point, b: Point, polygon: []Point, edges: []Segment) -> bool {
    min_x := min(a.x, b.x);
    max_x := max(a.x, b.x);

    min_y := min(a.y, b.y);
    max_y := max(a.y, b.y);

    top_left := Point.{min_x, min_y}; // top left
    top_right := Point.{max_x, min_y}; // top right

    bot_left := Point.{min_x, max_y}; // bot left
    bot_right := Point.{max_x, max_y}; // bot right

    if !move_is_allowed(top_left, top_right, polygon, edges) {
        return false;
    }
    if !move_is_allowed(bot_left, bot_right, polygon, edges) {
        return false;
    }
    if !move_is_allowed(top_left, bot_left, polygon, edges) {
        return false;
    }
    if !move_is_allowed(top_right, bot_right, polygon, edges) {
        return false;
    }

    return true;

}

task_two :: (lines: []string) -> int {
    points := parse(lines);
    edges := polygon_edges(points);
    m := 0;

    for i : 0..points.count - 1 {
        for j : i + 1..points.count - 1{
            if is_points_all_inside(points[i], points[j], points, edges) {
                m = max(m, point_square(points[i], points[j]));
            }
        }
    }
    return m;
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
