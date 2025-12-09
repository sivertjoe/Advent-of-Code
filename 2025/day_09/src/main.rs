fn parse(input: &[String]) -> Vec<(usize, usize)> {
    input
        .iter()
        .map(|line| {
            line.split_once(',')
                .map(|(i, j)| (i.parse::<usize>().unwrap(), j.parse::<usize>().unwrap()))
                .unwrap()
        })
        .collect::<Vec<_>>()
}

fn num_squares(a: (usize, usize), b: (usize, usize)) -> usize {
    let min_x = std::cmp::min(a.0, b.0);
    let max_x = std::cmp::max(a.0, b.0);

    let min_y = std::cmp::min(a.1, b.1);
    let max_y = std::cmp::max(a.1, b.1);

    let sz = (max_x - min_x + 1) * (max_y - min_y + 1);
    sz
}

fn task_one(input: &[String]) -> usize {
    let elems = parse(input);
    let mut max = 0;
    for i in 0..elems.len() {
        for j in i + 1..elems.len() {
            max = max.max(num_squares(elems[i], elems[j]));
        }
    }
    max
}

type Point = (usize, usize);
type Segment = (Point, Point);

fn orient(a: Point, b: Point, c: Point) -> i64 {
    let (ax, ay) = (a.0 as i64, a.1 as i64);
    let (bx, by) = (b.0 as i64, b.1 as i64);
    let (cx, cy) = (c.0 as i64, c.1 as i64);

    (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
}

fn on_segment(a: Point, b: Point, p: Point) -> bool {
    let (ax, ay) = (a.0 as i64, a.1 as i64);
    let (bx, by) = (b.0 as i64, b.1 as i64);
    let (px, py) = (p.0 as i64, p.1 as i64);

    if orient(a, b, p) != 0 {
        return false;
    }

    px >= ax.min(bx) && px <= ax.max(bx) && py >= ay.min(by) && py <= ay.max(by)
}

fn segments_cross_properly(a: Point, b: Point, c: Point, d: Point) -> bool {
    let o1 = orient(a, b, c);
    let o2 = orient(a, b, d);
    let o3 = orient(c, d, a);
    let o4 = orient(c, d, b);

    let ab_straddles_cd = (o1 < 0 && o2 > 0) || (o1 > 0 && o2 < 0);
    let cd_straddles_ab = (o3 < 0 && o4 > 0) || (o3 > 0 && o4 < 0);

    ab_straddles_cd && cd_straddles_ab
}

fn polygon_edges(vertices: &[Point]) -> Vec<Segment> {
    let n = vertices.len();
    let mut edges = Vec::with_capacity(n);
    for i in 0..n {
        let a = vertices[i];
        let b = vertices[(i + 1) % n]; // wrap last → first
        edges.push((a, b));
    }
    edges
}

fn point_on_polygon_edge(p: Point, vertices: &[Point]) -> bool {
    let edges = polygon_edges(vertices);
    edges.iter().any(|&(a, b)| on_segment(a, b, p))
}

fn point_in_or_on_polygon(p: Point, vertices: &[Point]) -> bool {
    if point_on_polygon_edge(p, vertices) {
        return true;
    }

    let (px, py) = (p.0 as f64, p.1 as f64);

    let mut inside = false;
    let n = vertices.len();
    for i in 0..n {
        let (x1, y1) = (vertices[i].0 as f64, vertices[i].1 as f64);
        let (x2, y2) = (
            vertices[(i + 1) % n].0 as f64,
            vertices[(i + 1) % n].1 as f64,
        );

        // Check if ray to the right from p intersects edge (x1,y1)-(x2,y2)
        let intersects = ((y1 > py) != (y2 > py)) && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1);

        if intersects {
            inside = !inside;
        }
    }
    inside
}

fn move_is_allowed(old_p: Point, new_p: Point, polygon: &[Point]) -> bool {
    if !point_in_or_on_polygon(old_p, polygon) {
        return false;
    }
    if !point_in_or_on_polygon(new_p, polygon) {
        return false;
    }

    let edges = polygon_edges(polygon);
    for &(c, d) in &edges {
        if segments_cross_properly(old_p, new_p, c, d) {
            return false;
        }
    }

    true
}

fn num_squares5(a: (usize, usize), b: (usize, usize), polygon: &[Point]) -> Option<usize> {
    let min_x = std::cmp::min(a.0, b.0);
    let max_x = std::cmp::max(a.0, b.0);

    let min_y = std::cmp::min(a.1, b.1);
    let max_y = std::cmp::max(a.1, b.1);

    let top_left = (min_x, min_y); // top left
    let top_right = (max_x, min_y); // top right

    let bot_left = (min_x, max_y); // bot left
    let bot_right = (max_x, max_y); // bot right

    if !move_is_allowed(top_left, top_right, polygon) {
        return None;
    }
    if !move_is_allowed(bot_left, bot_right, polygon) {
        return None;
    }
    if !move_is_allowed(top_left, bot_left, polygon) {
        return None;
    }
    if !move_is_allowed(top_right, bot_right, polygon) {
        return None;
    }

    let sz = (max_x - min_x + 1) * (max_y - min_y + 1);
    Some(sz)
}

fn task_two(input: &[String]) -> usize {
    let elems = parse(input);

    let mut max = 0;
    for i in 0..elems.len() {
        for j in i + 1..elems.len() {
            if let Some(sz) = num_squares5(elems[i], elems[j], &elems) {
                max = max.max(sz);
            }
        }
    }
    max
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

enum Task {
    One,
    Two,
}

fn time<F, T, U>(task: Task, f: F, arg: T)
where
    F: Fn(T) -> U,
    U: std::fmt::Display,
{
    let t = std::time::Instant::now();
    let res = f(arg);
    let elapsed = t.elapsed();
    let fmt = std::env::var("TASKUNIT").unwrap_or("ms".to_owned());

    let (u, elapsed) = match fmt.as_str() {
        "ms" => ("ms", elapsed.as_millis()),
        "ns" => ("ns", elapsed.as_nanos()),
        "us" => ("μs", elapsed.as_micros()),
        "s" => ("s", elapsed.as_secs() as u128),
        _ => panic!("unsupported time format"),
    };

    match task {
        Task::One => {
            println!("({}{u})\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}{u})\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
