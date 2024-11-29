use std::collections::*;

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Ord, PartialOrd)]
enum Dir {
    Right,
    Down,
    Left,
    Up,
}

fn neighbors(vec: &[Vec<u8>], p: (Dir, (usize, usize))) -> Vec<(Dir, (usize, usize))> {
    let mut res = Vec::new();

    let bounds = |y: isize, x: isize| {
        y >= 0 && y < vec.len() as isize && x >= 0 && x < vec[0].len() as isize
    };

    let (dir, pos) = p;
    let (y, x) = (pos.0 as isize, pos.1 as isize);

    match dir {
        Dir::Right => match vec[pos.0][pos.1] {
            b'.' | b'-' => res.push((Dir::Right, (y, x + 1))),
            b'\\' => res.push((Dir::Down, (y + 1, x))),
            b'/' => res.push((Dir::Up, (y - 1, x))),
            b'|' => {
                res.push((Dir::Down, (y + 1, x)));
                res.push((Dir::Up, (y - 1, x)));
            }
            _ => unreachable!(),
        },
        Dir::Down => match vec[pos.0][pos.1] {
            b'.' | b'|' => res.push((Dir::Down, (y + 1, x))),
            b'\\' => res.push((Dir::Right, (y, x + 1))),
            b'/' => res.push((Dir::Left, (y, x - 1))),
            b'-' => {
                res.push((Dir::Right, (y, x + 1)));
                res.push((Dir::Left, (y, x - 1)));
            }
            _ => unreachable!(),
        },
        Dir::Left => match vec[pos.0][pos.1] {
            b'.' | b'-' => res.push((Dir::Left, (y, x - 1))),
            b'\\' => res.push((Dir::Up, (y - 1, x))),
            b'/' => res.push((Dir::Down, (y + 1, x))),
            b'|' => {
                res.push((Dir::Up, (y - 1, x)));
                res.push((Dir::Down, (y + 1, x)));
            }
            _ => unreachable!(),
        },
        Dir::Up => match vec[pos.0][pos.1] {
            b'.' | b'|' => res.push((Dir::Up, (y - 1, x))),
            b'\\' => res.push((Dir::Left, (y, x - 1))),
            b'/' => res.push((Dir::Right, (y, x + 1))),
            b'-' => {
                res.push((Dir::Right, (y, x + 1)));
                res.push((Dir::Left, (y, x - 1)));
            }
            _ => unreachable!(),
        },
    }

    res.into_iter()
        .filter(|(_dir, (y, x))| bounds(*y, *x))
        .map(|(dir, (y, x))| (dir, (y as usize, x as usize)))
        .collect()
}

fn num_energized(vec: &[Vec<u8>], start: (Dir, (usize, usize))) -> usize {
    let mut seen = HashSet::with_hasher(fxhash::FxBuildHasher::default());
    seen.insert(start);

    let mut stack = vec![start];

    while let Some(p) = stack.pop() {
        for ns in neighbors(&vec, p) {
            if seen.insert(ns) {
                stack.push(ns);
            }
        }
    }

    seen.into_iter()
        .map(|(_d, p)| p)
        .collect::<HashSet<_>>()
        .len()
}

fn parse(input: &[String]) -> Vec<Vec<u8>> {
    input
        .iter()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>()
}

fn task_one(input: &[String]) -> usize {
    num_energized(&parse(input), (Dir::Right, (0, 0)))
}

fn task_two(input: &[String]) -> usize {
    let vec = parse(input);

    let mut max = 0;
    for y in 0..vec.len() {
        max = max.max(num_energized(&vec, (Dir::Left, (y, vec[0].len() - 1))));
        max = max.max(num_energized(&vec, (Dir::Right, (y, 0))));
    }

    for x in 0..vec[0].len() {
        max = max.max(num_energized(&vec, (Dir::Down, (0, x))));
        max = max.max(num_energized(&vec, (Dir::Up, (vec.len() - 1, x))));
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
