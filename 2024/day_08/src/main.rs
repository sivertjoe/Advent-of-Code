use std::collections::*;

type P = (isize, isize);

fn in_line2(a: P, b: P, i: isize) -> (P, P) {
    let disty = i * (a.0.abs_diff(b.0) as isize);
    let distx = i * (a.1.abs_diff(b.1) as isize);

    let dy = (a.0 - b.0).signum();
    let dx = (a.1 - b.1).signum();

    (
        (a.0 + dy * disty, a.1 + dx * distx),
        (b.0 - dy * disty, b.1 - dx * distx),
    )
}

fn solve(input: &[String], part_1: bool) -> usize {
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            if ch != '.' {
                let y = y as isize;
                let x = x as isize;
                map.insert((y, x), ch);
            }
        }
    }
    let max = ((input.len() - 1) as _, (input[0].len() - 1) as _);
    let mut set = HashSet::new();

    for (pos, ch) in map.iter() {
        for (p, c) in map.iter() {
            if *ch != *c || pos == p {
                continue;
            }

            for i in 1.. {
                let (a, b) = in_line2(*pos, *p, i);
                let mut fin = false;

                for p in [a, b]
                    .into_iter()
                    .filter(|p| p.0 >= 0 && p.0 <= max.0 && p.1 >= 0 && p.1 <= max.1)
                {
                    set.insert(p);
                    fin = true;
                }

                if part_1 || !fin {
                    break;
                }
            }
        }
    }

    if part_1 {
        set.len()
    } else {
        map.into_keys().collect::<HashSet<_>>().union(&set).count()
    }
}

fn task_one(input: &[String]) -> usize {
    solve(input, true)
}

fn task_two(input: &[String]) -> usize {
    solve(input, false)
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
