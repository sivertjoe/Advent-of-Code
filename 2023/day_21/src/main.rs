use std::collections::*;
type HashMap<K, V> = std::collections::HashMap<K, V, fxhash::FxBuildHasher>;

type Map = HashMap<(isize, isize), char>;

fn parse(input: &[String]) -> ((isize, isize), Map) {
    let mut start = (-1, -1);
    let mut map = HashMap::default();
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

fn bfs(map: &Map, start: (isize, isize)) -> HashMap<(isize, isize), usize> {
    let mut dist = HashMap::with_hasher(fxhash::FxBuildHasher::default());
    dist.insert(start, 0);

    let mut vec = VecDeque::new();
    vec.push_back((0, start));

    while let Some((cost, pos)) = vec.pop_front() {
        for ns in neighbors(map, pos) {
            dist.entry(ns).or_insert_with(|| {
                vec.push_back((cost + 1, ns));
                cost + 1
            });
        }
    }

    dist
}

fn task_one(input: &[String]) -> usize {
    let (start, map) = parse(input);
    let dist = bfs(&map, start);
    dist.into_values()
        .filter(|n| *n <= 64 && n % 2 == 0)
        .count()
}

fn task_two(input: &[String]) -> usize {
    let (start, map) = parse(input);
    let dist = bfs(&map, start);

    let even_corners = dist.values().filter(|v| **v % 2 == 0 && **v > 65).count();
    let odd_corners = dist.values().filter(|v| **v % 2 == 1 && **v > 65).count();

    let n = (26501365 - (input.len() / 2)) / input.len();
    assert_eq!(n, 202300);

    let even = n * n;
    let odd = (n + 1) * (n + 1);

    let odd_full = dist.values().filter(|v| **v % 2 == 1).count();
    let even_full = dist.values().filter(|v| **v % 2 == 0).count();

    odd * odd_full + even * even_full - ((n + 1) * odd_corners) + (n * even_corners)
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
