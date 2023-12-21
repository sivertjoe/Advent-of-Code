use std::collections::*;

type Map = HashMap<(isize, isize), char>;

fn parse(input: &[String]) -> ((isize, isize), Map) {
    let mut start = (-1, -1);
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            if ch == 'S' {
                start = (y as isize, x as isize);
                map.insert((y as isize, x as isize), '.');
            } else {
                map.insert((y as isize, x as isize), ch);
            }
        }
    }
    (start, map)
}

fn neighbors(map: &Map, pos: (isize, isize)) -> impl Iterator<Item = (isize, isize)> + '_ {
    let y = pos.0;
    let x = pos.1;

    [(y + 1, x), (y - 1, x), (y, x - 1), (y, x + 1)]
        .into_iter()
        .filter(|(y, x)| matches!(map.get(&(*y, *x)), Some(ch) if *ch == '.'))
}

fn bfs(map: &Map, start: (isize, isize), ss: isize) -> usize {
    let mut ans = HashSet::new();
    let mut seen = HashSet::new();
    seen.insert(start);

    let mut vec = VecDeque::new();
    vec.push_back((start, ss));

    while let Some((pos, s)) = vec.pop_front() {
        if s % 2 == 0 {
            ans.insert(pos);
        }
        if s == 0 {
            continue;
        }

        for ns in neighbors(map, pos) {
            if seen.insert(ns) {
                vec.push_back((ns, s - 1));
            }
        }
    }

    ans.len()
}

fn task_one(input: &[String]) -> usize {
    let (start, map) = parse(input);
    bfs(&map, start, 64)
}

fn task_two(input: &[String]) -> usize {
    let (start, map) = parse(input);
    math_brr(&map, input.len(), start, 26501365)
}

fn math_brr(map: &Map, size: usize, (sr, sc): (isize, isize), steps: usize) -> usize {
    let grid_width = (steps / size - 1) as usize;
    let size = size as isize;

    let odd = (grid_width / 2 * 2 + 1).pow(2) as usize;
    let even = ((grid_width + 1) / 2 * 2).pow(2) as usize;

    let odd_points = bfs(&map, (sr, sc), size * 2 + 1);
    let even_points = bfs(&map, (sr, sc), size * 2);

    let corner_t = bfs(&map, (size - 1, sc), size - 1);
    let corner_r = bfs(&map, (sr, 0), size - 1);
    let corner_b = bfs(&map, (0, sc), size - 1);
    let corner_l = bfs(&map, (sr, size - 1), size - 1);

    let small_tr = bfs(&map, (size - 1, 0), size / 2 - 1);
    let small_tl = bfs(&map, (size - 1, size - 1), size / 2 - 1);
    let small_br = bfs(&map, (0, 0), size / 2 - 1);
    let small_bl = bfs(&map, (0, size - 1), size / 2 - 1);

    let large_tr = bfs(&map, (size - 1, 0), size * 3 / 2 - 1);
    let large_tl = bfs(&map, (size - 1, size - 1), size * 3 / 2 - 1);
    let large_br = bfs(&map, (0, 0), size * 3 / 2 - 1);
    let large_bl = bfs(&map, (0, size - 1), size * 3 / 2 - 1);

    (odd as usize) * odd_points
        + even * even_points
        + corner_t
        + corner_r
        + corner_b
        + corner_l
        + (grid_width + 1) * (small_tr + small_tl + small_br + small_bl)
        + grid_width * (large_tr + large_tl + large_br + large_bl)
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
