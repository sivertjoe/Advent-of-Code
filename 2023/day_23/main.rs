use std::collections::*;

#[derive(Debug, Clone, Copy, PartialEq, Ord, PartialOrd, Eq, Hash)]
enum Dir {
    Up,
    Down,
}

fn get_dir(elem: u8, prev: (usize, usize), new: (usize, usize)) -> Dir {
    if new.1 != prev.1 {
        if new.1 < prev.1 {
            if elem == b'<' {
                Dir::Down
            } else {
                Dir::Up
            }
        } else {
            if elem == b'<' {
                Dir::Up
            } else {
                Dir::Down
            }
        }
    } else {
        if new.0 < prev.0 {
            if elem == b'^' {
                Dir::Down
            } else {
                Dir::Up
            }
        } else {
            if elem == b'^' {
                Dir::Up
            } else {
                Dir::Down
            }
        }
    }
}

fn neighbors(vec: &[Vec<u8>], pos: (usize, usize)) -> Vec<((usize, usize))> {
    let mut res = Vec::new();

    if matches!(vec[pos.0][pos.1], b'>' | b'^' | b'v' | b'<') {
        let n = match vec[pos.0][pos.1] {
            b'>' => (pos.0, pos.1 + 1),
            b'<' => (pos.0, pos.1 - 1),
            b'^' => (pos.0 - 1, pos.1),
            b'v' => (pos.0 + 1, pos.1),
            _ => unreachable!(),
        };
        res.push(n);
        return res;
    }

    let y = pos.0 as isize;
    let x = pos.1 as isize;

    for (y, x) in [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)] {
        if !(y >= 0 && y < vec.len() as isize && x >= 0 && x < vec[0].len() as isize) {
            continue;
        }
        let y = y as usize;
        let x = x as usize;

        let elem = vec[y][x];
        match elem {
            b'#' => continue,
            b'.' => {
                res.push((y, x));
            }
            b'>' | b'<' | b'^' | b'v' => {
                let dir = get_dir(elem, pos, (y, x));
                if matches!(dir, Dir::Up) {
                    continue;
                }
                /*match (dir, state) {
                    (_, None) => {
                        res.push(((y, x), Some(dir)));
                    }
                    (Dir::Up, Some(Dir::Down)) => {
                        res.push(((y, x), Some(dir)));
                    }
                    (Dir::Down, Some(Dir::Up)) => {
                        res.push(((y, x), Some(dir)));
                    }
                    _ => {}
                };*/
                res.push((y, x));
            }
            _ => unreachable!(),
        }
    }

    res
}

fn explore(
    vec: &[Vec<u8>],
    pos: (usize, usize),
    cost: usize,
    mut seen: HashSet<(usize, usize)>,
) -> Option<usize> {
    let mut stack = Vec::new();
    stack.push((cost, pos));

    seen.insert(pos);

    let end = (vec.len() - 1, vec[0].len() - 2);

    let mut longest = None;

    while let Some((cost, pos)) = stack.pop() {
        if pos == end {
            longest = match longest {
                None => Some(cost),
                Some(cc) => Some(cost.max(cc)),
            };
            continue;
        }

        for ns in neighbors(&vec, pos) {
            if vec[ns.0][ns.1] == b'.' {
                if seen.insert(ns) {
                    stack.push((cost + 1, ns));
                }
            }
            if !seen.contains(&ns) {
                match explore(vec, ns, cost + 1, seen.clone()) {
                    None => {}
                    Some(len) => match longest {
                        None => {
                            longest = Some(len);
                        }
                        Some(dist) => {
                            longest = Some(dist.max(len));
                        }
                    },
                }
            }
        }
    }

    longest
}

fn task_one(input: &[String]) -> usize {
    let vec = input
        .iter()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let start = (0, 1);
    explore(&vec, start, 0, HashSet::new()).unwrap()
}

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
