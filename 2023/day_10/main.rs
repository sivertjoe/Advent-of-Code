use std::collections::*;

fn parse(input: &[String]) -> Vec<Vec<u8>> {
    input.iter().map(|line| line.as_bytes().to_vec()).collect()
}

fn start(vec: &[Vec<u8>]) -> (usize, usize) {
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if vec[y][x] == b'S' {
                return (x, y);
            }
        }
    }
    unreachable!()
}

fn figure_out_start(vec: &[Vec<u8>], pos: (usize, usize)) -> u8 {
    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let x = pos.0 as isize;
    let y = pos.1 as isize;

    let bounds = |tx, ty| tx >= 0 && tx < mx && ty >= 0 && ty < my;

    if bounds(x, y - 1)
        && bounds(x, y + 1)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'|', b'L', b'J'].contains(&vec[pos.1 + 1][pos.0])
    {
        return b'|';
    }
    if bounds(x - 1, y)
        && bounds(x + 1, y)
        && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'-';
    }
    if bounds(x, y - 1)
        && bounds(x + 1, y)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'L';
    }
    if bounds(x, y - 1)
        && bounds(x - 1, y)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1])
    {
        return b'J';
    }
    if bounds(x, y + 1)
        && bounds(x + 1, y)
        && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'F';
    }

    if bounds(x - 1, y)
        && bounds(x, y + 1)
        && [b'-', b'F', b'L'].contains(&vec[pos.1][pos.0 - 1])
        && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0])
    {
        return b'7';
    }
    unreachable!()
}

fn neighbors(vec: &[Vec<u8>], pos: (usize, usize)) -> Vec<(usize, usize)> {
    let me = vec[pos.1][pos.0];

    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let x = pos.0 as isize;
    let y = pos.1 as isize;

    let bounds = |tx, ty| tx >= 0 && tx < mx && ty >= 0 && ty < my;

    let mut ns = Vec::new();
    match me {
        b'|' => {
            if bounds(x, y - 1) && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0]) {
                ns.push((pos.0, pos.1 - 1));
            }
            if bounds(x, y + 1) && [b'|', b'L', b'J'].contains(&vec[pos.1 + 1][pos.0]) {
                ns.push((pos.0, pos.1 + 1));
            }
        }
        b'-' => {
            if bounds(x - 1, y) && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1]) {
                ns.push((pos.0 - 1, pos.1));
            }
            if bounds(x + 1, y) && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1]) {
                ns.push((pos.0 + 1, pos.1));
            }
        }
        b'L' => {
            if bounds(x, y - 1) && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0]) {
                ns.push((pos.0, pos.1 - 1));
            }
            if bounds(x + 1, y) && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1]) {
                ns.push((pos.0 + 1, pos.1));
            }
        }
        b'J' => {
            if bounds(x, y - 1) && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0]) {
                ns.push((pos.0, pos.1 - 1));
            }
            if bounds(x - 1, y) && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1]) {
                ns.push((pos.0 - 1, pos.1));
            }
        }
        b'7' => {
            if bounds(x - 1, y) && [b'-', b'F', b'L'].contains(&vec[pos.1][pos.0 - 1]) {
                ns.push((pos.0 - 1, pos.1));
            }
            if bounds(x, y + 1) && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0]) {
                ns.push((pos.0, pos.1 + 1));
            }
        }
        b'F' => {
            if bounds(x, y + 1) && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0]) {
                ns.push((pos.0, pos.1 + 1));
            }
            if bounds(x + 1, y) && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1]) {
                ns.push((pos.0 + 1, pos.1));
            }
        }
        _ => unreachable!(),
    };
    ns
}

fn solve(vec: &[Vec<u8>], start: (usize, usize)) -> HashMap<(usize, usize), usize> {
    let mut dist = HashMap::new();
    dist.insert(start, 0);

    let mut heap = BinaryHeap::new();

    heap.push((0, start));

    while let Some((cost, pos)) = heap.pop() {
        if cost > dist[&pos] {
            continue;
        }

        for pair in neighbors(vec, pos) {
            let temp = dist.entry(pair).or_insert(usize::MAX);
            if cost + 1 < *temp {
                *temp = cost + 1;
                heap.push((cost + 1, pair));
            }
        }
    }
    // dist.into_values().max().unwrap()
    dist
}

fn task_one(input: &[String]) -> usize {
    let mut vec = parse(input);
    let start = start(&vec);
    let ch = figure_out_start(&vec, start);
    vec[start.1][start.0] = ch;
    solve(&vec, start).into_values().max().unwrap()
}

fn expand_char(
    center: (usize, usize),
    xy: (usize, usize),
    ch: u8,
    new: &mut [Vec<u8>],
    other: &[Vec<u8>],
) {
    match ch {
        b'.' => {
            new[center.1][center.0] = b'.';
        }
        b'S' => {
            let ch = figure_out_start(&other, xy);
            expand_char(center, xy, ch, new, other);
            new[center.1][center.0] = b'S'; // center
        }
        b'-' => {
            new[center.1][center.0 - 1] = b'-';
            new[center.1][center.0] = b'-';
            new[center.1][center.0 + 1] = b'-';
        }
        b'|' => {
            new[center.1 - 1][center.0] = b'|';
            new[center.1][center.0] = b'|';
            new[center.1 + 1][center.0] = b'|';
        }
        b'F' => {
            new[center.1][center.0 + 1] = b'-';
            new[center.1][center.0] = b'F';
            new[center.1 + 1][center.0] = b'|';
        }
        b'7' => {
            new[center.1][center.0 - 1] = b'-';
            new[center.1][center.0] = b'7';
            new[center.1 + 1][center.0] = b'|';
        }
        b'L' => {
            new[center.1][center.0 + 1] = b'-';
            new[center.1][center.0] = b'L';
            new[center.1 - 1][center.0] = b'|';
        }
        b'J' => {
            new[center.1][center.0 - 1] = b'-';
            new[center.1][center.0] = b'J';
            new[center.1 - 1][center.0] = b'|';
        }

        _ => unreachable!(),
    }
}

fn expand(vec: &[Vec<u8>]) -> Vec<Vec<u8>> {
    let mut new = vec![vec![b'x'; 3 * vec[0].len()]; 3 * vec.len()];

    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            let elem = vec[y][x];
            let center = (3 * x + 1, 3 * y + 1);
            expand_char(center, (x, y), elem, &mut new, &vec);
        }
    }

    new
}

fn _find_start(new: &[Vec<u8>]) -> (usize, usize) {
    for y in 0..new.len() {
        for x in 0..new[0].len() {
            if new[y][x] == b'S' {
                return (x, y);
            }
        }
    }
    unreachable!()
}

fn find_point(new: &[Vec<u8>], dist: &HashMap<(usize, usize), usize>) -> (usize, usize) {
    for y in 0..new.len() {
        for x in 0..new[0].len() - 1 {
            let elem = new[y][x + 1];

            if new[y][x] == b'|' && dist.contains_key(&(x, y)) && (elem == b'.' || elem == b'x') {
                return (x + 1, y);
            }
        }
    }
    unreachable!()
}

fn task_two(input: &[String]) -> usize {
    let vec = parse(input);

    let mut new = expand(&vec);
    let astart = _find_start(&new);

    let start = start(&vec);
    let ch = figure_out_start(&vec, start);

    new[astart.1][astart.0] = ch;
    let dist = solve(&new, astart);

    let mut mapping = HashMap::new();
    let start = find_point(&new, &dist);
    mapping.insert(start, new[start.1][start.0]);
    let mut stack = vec![start];

    while let Some(p) = stack.pop() {
        for p in [
            (p.0 + 1, p.1),
            (p.0 - 1, p.1),
            (p.0, p.1 + 1),
            (p.0, p.1 - 1),
        ] {
            let elem = new[p.1][p.0];
            if !dist.contains_key(&p) && mapping.insert(p, elem).is_none() {
                stack.push(p);
            }
        }
    }

    let mut dot = 0;
    let mut sym = 0;
    for elem in mapping.into_values().filter(|elem| *elem != b'x') {
        match elem {
            b'.' => dot += 1,
            _ if [b'|', b'-', b'7', b'F', b'J', b'L'].contains(&elem) => {
                sym += 1;
            }
            _ => {}
        }
    }
    dot + (sym / 3)
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
