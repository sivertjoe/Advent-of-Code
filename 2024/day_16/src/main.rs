use std::cmp::Reverse;
use std::collections::*;

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

fn task_one(input: &[String]) -> usize {
    let map: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();

    let mut scores: HashMap<(usize, usize), usize> = HashMap::new();

    let mut start = (0, 0);
    let mut stop = (0, 0);
    for (y, line) in map.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            if *ch == b'S' {
                start = (y, x);
            }
            if *ch == b'E' {
                stop = (y, x);
            }
        }
    }
    use std::cmp::Reverse;

    let mut vec = BinaryHeap::new();
    let mut seen = HashSet::new();
    vec.push((Reverse(0), start, Some((0, 1))));
    seen.insert(start);

    while let Some((cost, pos, direction)) = vec.pop() {
        if pos == stop {
            return cost.0;
        }

        for neighbor in neighbors(&map, pos).filter(|next| map[next.0][next.1] != b'#') {
            if seen.insert(neighbor) {
                let new_dir = Some(get_direction(pos, neighbor));
                let new_cost = if new_dir.is_some() && new_dir != direction {
                    Reverse(cost.0 + 1000 + 1)
                } else {
                    Reverse(cost.0 + 1)
                };

                vec.push((new_cost, neighbor, new_dir));
            }
        }
    }
    unreachable!()
}

fn printerino(map: &[Vec<u8>], seen: &BTreeSet<(usize, usize)>) {
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

fn best_path(map: &[Vec<u8>], start: (usize, usize), _dir: (isize, isize)) -> usize {
    let mut stop = (0, 0);
    for (y, line) in map.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            if *ch == b'E' {
                stop = (y, x);
            }
        }
    }

    let mut vec = BinaryHeap::new();
    let mut seen = HashSet::new();
    vec.push((Reverse(0), start, Some(_dir)));
    seen.insert(start);

    while let Some((cost, pos, direction)) = vec.pop() {
        if pos == stop {
            return cost.0;
        }

        for neighbor in neighbors(&map, pos).filter(|next| map[next.0][next.1] != b'#') {
            if seen.insert(neighbor) {
                let new_dir = Some(get_direction(pos, neighbor));
                let new_cost = if new_dir.is_some() && new_dir != direction {
                    Reverse(cost.0 + 1000 + 1)
                } else {
                    Reverse(cost.0 + 1)
                };

                vec.push((new_cost, neighbor, new_dir));
            }
        }
    }
    unreachable!()
}

fn task_two(input: &[String]) -> usize {
    let map: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();
    let mut dist = HashMap::new();

    println!("CREATING DIST MAP");
    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] != b'#' {
                let min = [(1, 0), (-1, 0), (0, 1), (0, -1)]
                    .into_iter()
                    .map(|dir| best_path(&map, (y, x), dir))
                    .min()
                    .unwrap();
                dist.insert((y, x), min);
            }
        }
    }
    println!("DONE");

    let mut scores = Vec::new();

    let mut start = (0, 0);
    let mut stop = (0, 0);
    for (y, line) in map.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            if *ch == b'S' {
                start = (y, x);
            }
            if *ch == b'E' {
                stop = (y, x);
            }
        }
    }

    let mut vec = BinaryHeap::new();
    let mut lseen = BTreeSet::new();
    lseen.insert(start);

    vec.push((Reverse(0), start, Some((0, 1)), lseen));

    let best_score = task_one(input);

    println!("SEARCHING....");
    while let Some((cost, pos, direction, seen)) = vec.pop() {
        // println!("{:?}", pos);
        // printerino(&map, &seen);
        // let _ = std::io::stdin().read_line(&mut String::new());
        if pos == stop {
            scores.push(seen);
            let len = scores.iter().flatten().collect::<HashSet<_>>().len();
            println!("{}", len);
            continue;
        }

        let can_get = cost.0 + dist.get(&pos).unwrap();

        if can_get > best_score {
            continue;
        }

        for neighbor in neighbors(&map, pos).filter(|next| map[next.0][next.1] != b'#') {
            if !seen.contains(&neighbor) {
                let mut new_seen = seen.clone();
                new_seen.insert(neighbor);
                let new_dir = Some(get_direction(pos, neighbor));
                let new_cost = if new_dir.is_some() && new_dir != direction {
                    Reverse(cost.0 + 1000 + 1)
                } else {
                    Reverse(cost.0 + 1)
                };
                vec.push((new_cost, neighbor, new_dir, new_seen));
            }
        }
    }
    //let vec = scores.into_iter().min_by_key(|k| k.0).unwrap().1;
    let set = scores.into_iter().flatten().collect::<HashSet<_>>();
    //printerino(&map, &set);
    set.len()
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
