use rayon::prelude::*;
use std::cmp::Reverse;
use std::collections::*;

type HashMap<K, V> = fxhash::FxHashMap<K, V>;

type P = (usize, usize);
type I = (isize, isize);

fn parse(input: &[String]) -> (Vec<Vec<u8>>, P, P) {
    let vec: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();
    let start = vec
        .iter()
        .flatten()
        .enumerate()
        .find_map(|(i, ch)| (*ch == b'S').then_some((i / vec.len(), i % vec.len())))
        .unwrap();

    let end = vec
        .iter()
        .flatten()
        .enumerate()
        .find_map(|(i, ch)| (*ch == b'E').then_some((i / vec.len(), i % vec.len())))
        .unwrap();

    (vec, start, end)
}

fn neighbors(map: &[Vec<u8>], (y, x): (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    let x = x as isize;
    let y = y as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(x, y)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(x, y)| (x as usize, y as usize))
}

fn get_direction(from: (usize, usize), to: (usize, usize)) -> (isize, isize) {
    let dx = to.0 as isize - from.0 as isize;
    let dy = to.1 as isize - from.1 as isize;

    let x_dir = dx.clamp(-1, 1);
    let y_dir = dy.clamp(-1, 1);

    (x_dir, y_dir)
}

fn dijkstra(vec: &[Vec<u8>], start: P, dir: I, end: P) -> HashMap<P, usize> {
    let mut dist = HashMap::default();

    let mut heap = BinaryHeap::new();
    dist.insert(start, 0);
    heap.push((Reverse(0), start, dir));

    while let Some((cost, pos, dir)) = heap.pop() {
        if cost.0 > *dist.get(&pos).unwrap_or(&usize::MAX) {
            continue;
        }

        if pos == end {
            return dist;
        }

        for n in neighbors(vec, pos).filter(|next| vec[next.0][next.1] != b'#') {
            let new_dir = get_direction(pos, n);
            let new_cost = cost.0 + if new_dir != dir { 1000 + 1 } else { 1 };

            if new_cost < *dist.get(&n).unwrap_or(&usize::MAX) {
                heap.push((Reverse(new_cost), n, new_dir));
                dist.insert(n, new_cost);
            }
        }
    }

    unreachable!()
}

fn task_one(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);
    let dist = dijkstra(&vec, start, (0, 1), end);
    dist[&end]
}

fn task_two(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);

    let dists = neighbors(&vec, end)
        .filter(|n| vec[n.0][n.1] != b'#')
        .par_bridge()
        .map(|next| {
            let dir = get_direction(end, next);
            dijkstra(&vec, end, dir, start)
        })
        .collect::<Vec<_>>();

    let from_start = dijkstra(&vec, start, (0, 1), end);
    let best_score = from_start[&end];

    let mut paths = 0;
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            let pos = &(y, x);
            if vec[y][x] == b'#' {
                continue;
            }
            if dists.iter().any(|dist| {
                from_start.get(pos).unwrap_or(&(usize::MAX / 2))
                    + dist.get(pos).unwrap_or(&(usize::MAX / 2))
                    == best_score
            }) {
                paths += 1;
            }
        }
    }

    paths
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
