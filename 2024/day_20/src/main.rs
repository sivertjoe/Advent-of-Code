use std::cmp::Reverse;
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

fn dijkstra(vec: &[Vec<u8>], start: P, end: P) -> HashMap<P, usize> {
    let mut dist = HashMap::default();

    let mut heap = BinaryHeap::new();
    dist.insert(start, 0);
    heap.push((Reverse(0), start));

    while let Some((cost, pos)) = heap.pop() {
        if cost.0 > *dist.get(&pos).unwrap_or(&usize::MAX) {
            continue;
        }

        for n in neighbors(vec, pos).filter(|next| vec[next.0][next.1] != b'#') {
            let new_cost = cost.0 + 1;

            if new_cost < *dist.get(&n).unwrap_or(&usize::MAX) {
                heap.push((Reverse(new_cost), n));
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
    let mut to_return = Vec::new();

    let mut seen = HashSet::new();
    seen.insert(pos);

    let mut points = vec![pos];
    for _ in 0..N {
        let mut new = Vec::new();
        for point in points {
            for dir in [(0, -1), (0, 1), (1, 0), (-1, 0)] {
                if let Some(next) = p(map, point, dir) {
                    if seen.insert(next) {
                        if map[next.0][next.1] != b'#' {
                            to_return.push(next);
                        }
                        new.push(next);
                    }
                }
            }
        }
        points = new;
    }

    to_return
}

fn cheat_walk<const N: usize>(
    map: &[Vec<u8>],
    start: P,
    steps: usize,
    from_end: &HashMap<P, usize>,
) -> Vec<usize> {
    let mut seen = HashSet::new();
    seen.insert(start);

    let mut vec = BinaryHeap::new();
    vec.push((Reverse(0), start));

    while let Some((cost, pos)) = vec.pop() {
        if cost.0 == steps {
            return get_cheatable_posistions::<N>(map, pos)
                .into_iter()
                .map(|p| cost.0 + manhatten_distance(pos, p) + from_end[&p])
                .collect();
        }

        if cost.0 > steps {
            continue;
        }

        for n in neighbors(&map, pos) {
            if map[n.0][n.1] == b'#' {
                continue;
            }
            let new_cost = Reverse(cost.0 + 1);
            if seen.insert(n) {
                vec.push((new_cost, n));
            }
        }
    }

    unreachable!()
}

fn task_one(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);

    let from_end = dijkstra(&vec, end, start);

    let best = from_end[&start];

    let limit = 100;

    let mut count = 0;
    for i in 0..best {
        for cost in cheat_walk::<2>(&vec, start, i, &from_end) {
            if cost < best && best - cost >= limit {
                count += 1;
            }
        }
    }
    count
}

fn task_two(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);

    let from_end = dijkstra(&vec, end, start);

    let best = from_end[&start];

    let limit = 100;

    let mut count = 0;
    for i in 0..best {
        for cost in cheat_walk::<20>(&vec, start, i, &from_end) {
            if cost < best && best - cost >= limit {
                count += 1;
            }
        }
    }
    count
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
