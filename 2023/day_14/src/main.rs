use std::collections::*;

fn task_one(input: &[String]) -> usize {
    let vec = input
        .iter()
        .map(|l| l.bytes().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut new = vec.clone();

    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            match new[y][x] {
                b'O' => {
                    new[y][x] = b'.';
                    let mut ny = (y as isize) - 1;
                    while ny >= 0 && new[ny as usize][x] == b'.' {
                        ny -= 1;
                    }
                    ny += 1;
                    new[ny as usize][x] = b'O';
                }
                ch => new[y][x] = ch,
            };
        }
    }
    let mut count = 0;
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if new[y][x] == b'O' {
                count += vec.len() - y;
            }
        }
    }

    count
}

fn cylcle(new: &mut [Vec<u8>]) {
    // north
    for y in 0..new.len() {
        for x in 0..new[0].len() {
            match new[y][x] {
                b'O' => {
                    new[y][x] = b'.';
                    let mut ny = (y as isize) - 1;
                    while ny >= 0 && new[ny as usize][x] == b'.' {
                        ny -= 1;
                    }
                    ny += 1;
                    new[ny as usize][x] = b'O';
                }
                ch => new[y][x] = ch,
            };
        }
    }

    // west
    for y in 0..new.len() {
        for x in 0..new[0].len() {
            match new[y][x] {
                b'O' => {
                    new[y][x] = b'.';
                    let mut nx = (x as isize) - 1;
                    while nx >= 0 && new[y][nx as usize] == b'.' {
                        nx -= 1;
                    }
                    nx += 1;
                    new[y][nx as usize] = b'O';
                }
                ch => new[y][x] = ch,
            };
        }
    }

    // south
    for y in (0..new.len()).rev() {
        for x in (0..new[0].len()).rev() {
            match new[y][x] {
                b'O' => {
                    new[y][x] = b'.';
                    let mut ny = (y as isize) + 1;
                    while (ny as usize) < new.len() && new[ny as usize][x] == b'.' {
                        ny += 1;
                    }
                    ny -= 1;
                    new[ny as usize][x] = b'O';
                }
                ch => new[y][x] = ch,
            };
        }
    }

    // east
    for y in (0..new.len()).rev() {
        for x in (0..new[0].len()).rev() {
            match new[y][x] {
                b'O' => {
                    new[y][x] = b'.';
                    let mut nx = (x as isize) + 1;
                    while (nx as usize) < new[0].len() && new[y][nx as usize] == b'.' {
                        nx += 1;
                    }
                    nx -= 1;
                    new[y][nx as usize] = b'O';
                }
                ch => new[y][x] = ch,
            };
        }
    }
}

fn task_two(input: &[String]) -> usize {
    let mut vec = input
        .iter()
        .map(|l| l.bytes().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut set = HashSet::new();

    let mut seen = Vec::new();

    seen.push(vec.clone());
    for _i in 0..1000000000 {
        cylcle(&mut vec);

        if !set.insert(vec.clone()) {
            break;
        }
        seen.push(vec.clone());
    }

    let start = seen.iter().position(|state| *state == vec).unwrap();

    let new: &[Vec<Vec<u8>>] = &seen[start..];

    let idx = (1000000000 - seen.len()) % new.len();
    let vec = new.get(idx).unwrap();

    let mut count = 0;
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if vec[y][x] == b'O' {
                count += vec.len() - y;
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
