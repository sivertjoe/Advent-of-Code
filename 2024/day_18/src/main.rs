use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::usize;

const H: usize = 70 + 1;
const W: usize = 70 + 1;

fn parse(input: &[String]) -> (Vec<Vec<usize>>, Vec<(usize, usize)>) {
    let vec = vec![vec![usize::MAX; W]; H];

    let bytes = input
        .iter()
        .map(|byte| {
            byte.split_once(',')
                .map(|(x, y)| (y.parse::<usize>().unwrap(), x.parse::<usize>().unwrap()))
                .unwrap()
        })
        .collect::<Vec<_>>();
    (vec, bytes)
}

fn neighbors<T>(map: &[Vec<T>], (y, x): (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    let x = x as isize;
    let y = y as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(y, x)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(y, x)| (y as usize, x as usize))
}

fn bfs(map: &[Vec<usize>], i: usize) -> Option<usize> {
    let mut seen =
        fxhash::FxHashSet::with_capacity_and_hasher(100, fxhash::FxBuildHasher::default());

    let mut vec = BinaryHeap::new();
    vec.push((Reverse(0), (0, 0)));

    while let Some((cost, pos)) = vec.pop() {
        if pos == (H - 1, W - 1) {
            return Some(cost.0);
        }

        for n in neighbors(&map, pos).filter(|next| map[next.0][next.1] > i) {
            let new_cost = Reverse(cost.0 + 1);
            if seen.insert(n) {
                vec.push((new_cost, n));
            }
        }
    }

    None
}

fn task_one(input: &[String]) -> usize {
    let (mut vec, bytes) = parse(input);
    for (i, (y, x)) in bytes.into_iter().take(1024).enumerate() {
        vec[y][x] = i + 1;
    }

    bfs(&vec, 1024).unwrap()
}

fn task_two(input: &[String]) -> String {
    let (mut vec, bytes) = parse(input);

    for (i, (y, x)) in bytes.iter().enumerate() {
        vec[*y][*x] = i + 1;
    }

    let ids = (1..=bytes.len()).collect::<Vec<_>>();
    let i = ids.partition_point(|i| bfs(&vec, *i).is_some());

    let (y, x) = &bytes[i];
    format!("{x},{y}")
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
