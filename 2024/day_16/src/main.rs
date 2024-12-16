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

fn dijkstra(vec: &[Vec<u8>], start: P, dir: I, _end: P) -> (I, HashMap<P, usize>) {
    let mut dist = HashMap::default();

    let mut heap = BinaryHeap::new();
    dist.insert(start, 0);
    heap.push((Reverse(0), start, dir));

    while let Some((cost, pos, dir)) = heap.pop() {
        if cost.0 > *dist.get(&pos).unwrap_or(&usize::MAX) {
            continue;
        }

        if pos == _end {
            return (dir, dist);
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

fn solve_from(
    vec: &[Vec<u8>],
    dists: &[HashMap<P, usize>],
    cost: usize,
    (from, dir): (P, I),
    end: P,
) -> HashSet<P> {
    let mut heap = BinaryHeap::new();
    let mut lseen = BTreeSet::new();
    lseen.insert(from);

    heap.push((Reverse(cost), from, dir, lseen));

    let best_score = dists[0][&from];

    let mut paths = HashSet::new();

    while let Some((cost, pos, direction, seen)) = heap.pop() {
        if pos == end {
            paths.extend(seen);
            continue;
        }

        if dists
            .iter()
            .all(|dist| cost.0 + dist.get(&pos).unwrap() > best_score)
        {
            continue;
        }

        for neighbor in neighbors(&vec, pos).filter(|next| vec[next.0][next.1] != b'#') {
            if !seen.contains(&neighbor) {
                let mut new_seen = seen.clone();
                new_seen.insert(neighbor);
                let new_dir = get_direction(pos, neighbor);
                let new_cost = if new_dir != direction {
                    Reverse(cost.0 + 1000 + 1)
                } else {
                    Reverse(cost.0 + 1)
                };
                heap.push((new_cost, neighbor, new_dir, new_seen));
            }
        }
    }
    paths
}

fn pr(map: &[Vec<u8>], seen: &HashSet<(usize, usize)>) {
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if seen.contains(&(y, x)) {
                print!("O");
            } else {
                print!("{}", map[y][x] as char);
            }
        }
        println!();
    }
}

fn task_one(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);
    let dist = dijkstra(&vec, start, (0, 1), end).1;
    dist[&end]
}

fn task_two(input: &[String]) -> usize {
    let (vec, start, end) = parse(input);

    let dists = neighbors(&vec, end)
        .filter(|n| vec[n.0][n.1] != b'#')
        .par_bridge()
        .map(|next| {
            let dir = get_direction(end, next);
            let (dir, mut dist) = dijkstra(&vec, end, dir, start);
            if dir != (0, -1) {
                // *dist.get_mut(&start).unwrap() += 1000;
                // dist.iter_mut()
                //     .filter(|(pos, _)| **pos != start)
                //     .for_each(|(_pos, v)| *v += 1000);
                println!("test..");
            }
            dist
        })
        .collect::<Vec<_>>();
    // println!("{}", dists.len());

    let from_start = dijkstra(&vec, start, (0, 1), end).1;
    let best_score = from_start[&end];

    // println!("best is {}", best_score);
    // for dist in &dists {
    //     println!("{}", from_start[&end] + dist[&end]);
    // }

    let mut uh = HashSet::new();
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
                uh.insert((y, x));
            }
        }
    }

    // let solve = solve_from(&vec, &dists, 0, (start, (0, 1)), end);
    // println!("uh {} solve {}", uh.len(), solve.len());
    // for u in uh {
    //     if !solve.contains(&u) {
    //         println!("{:?}", u);
    //     }
    // }
    pr(&vec, &uh);

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
