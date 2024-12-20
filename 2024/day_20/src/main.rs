use rayon::prelude::*;
use std::collections::*;

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

fn neighbors<T>(map: &[Vec<T>], (y, x): (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    let x = x as isize;
    let y = y as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(x, y)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(y, x)| (y as usize, x as usize))
}

fn dijkstra(map: &[Vec<u8>], start: P, _end: P) -> HashMap<P, usize> {
    let mut dist = HashMap::default();

    let mut vec = VecDeque::new();
    dist.insert(start, 0);
    vec.push_back((0, start));

    while let Some((cost, pos)) = vec.pop_front() {
        if cost > *dist.get(&pos).unwrap_or(&usize::MAX) {
            continue;
        }

        for n in neighbors(map, pos).filter(|next| map[next.0][next.1] != b'#') {
            let new_cost = cost + 1;
            if new_cost < *dist.get(&n).unwrap_or(&usize::MAX) {
                vec.push_back((new_cost, n));
                dist.insert(n, new_cost);
            }
        }
    }

    dist
}

fn in_bounds(p: I, map: &[Vec<u8>]) -> bool {
    p.0 >= 0 && p.0 < map.len() as isize && p.1 >= 0 && p.1 < map[0].len() as isize
}

fn p(vec: &[Vec<u8>], curr: P, dir: I) -> Option<P> {
    let next = (curr.0 as isize + dir.0, curr.1 as isize + dir.1);
    in_bounds(next, vec).then(|| (next.0 as usize, next.1 as usize))
}

fn manhatten_distance(p1: P, p2: P) -> usize {
    p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1)
}

fn get_cheatable_posistions<const N: usize>(map: &[Vec<u8>], pos: P) -> Vec<P> {
    let mut to_return = Vec::with_capacity(64);
    let directions = [(0isize, 1isize), (1, 0), (0, -1), (-1, 0)]; // up, right, down, left
    let mut seen = HashSet::new();
    let mut vec = VecDeque::new();

    vec.push_back((0, pos));
    seen.insert(pos);

    while let Some((cost, pos)) = vec.pop_front() {
        if cost < N {
            for dir in &directions {
                if let Some(next) = p(&map, pos, *dir) {
                    if seen.insert(next) {
                        vec.push_back((cost + 1, next));
                        if map[next.0][next.1] != b'#' {
                            to_return.push(next);
                        }
                    }
                }
            }
        }
    }

    to_return
}

fn cheat_walk<const N: usize>(
    map: &[Vec<u8>],
    start: P,
    steps: usize,
    from_end: &HashMap<P, usize>,
    limit: usize,
) -> usize {
    let best = from_end[&start];
    let mut seen = HashSet::new();
    seen.insert(start);

    let mut vec = VecDeque::new();
    vec.push_back((0, start));

    while let Some((cost, pos)) = vec.pop_front() {
        if cost == steps {
            return get_cheatable_posistions::<N>(map, pos)
                .into_iter()
                .map(|p| cost + manhatten_distance(pos, p) + from_end[&p])
                .filter(|cost| *cost < best && (best - cost) >= limit)
                .count();
        }

        for n in neighbors(&map, pos) {
            if map[n.0][n.1] == b'#' {
                continue;
            }
            let new_cost = cost + 1;
            if seen.insert(n) {
                vec.push_back((new_cost, n));
            }
        }
    }

    unreachable!()
}

fn solve<const N: usize>(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);
    let from_end = dijkstra(&vec, end, start);
    let best = from_end[&start];

    let limit = 100;

    (0..best)
        .into_par_iter()
        .map(|i| cheat_walk::<N>(&vec, start, i, &from_end, limit))
        .sum()
}

fn task_one(input: &[String]) -> usize {
    solve::<2>(input)
}

fn task_two(input: &[String]) -> usize {
    solve::<20>(input)
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
