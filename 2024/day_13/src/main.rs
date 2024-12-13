use std::collections::*;

fn split(line: &str) -> (usize, usize) {
    let fst1 = line.chars().position(|ch| ch == 'X').unwrap();
    let fst2 = line.chars().position(|ch| ch == ',').unwrap();

    let fst = line[fst1 + 2..fst2].parse::<usize>().unwrap();

    let snd1 = line.chars().position(|ch| ch == 'Y').unwrap();

    let snd = line[snd1 + 2..].parse::<usize>().unwrap();

    (fst, snd)
}
fn split2(line: &str) -> I {
    let fst1 = line.chars().position(|ch| ch == 'X').unwrap();
    let fst2 = line.chars().position(|ch| ch == ',').unwrap();

    let fst = line[fst1 + 2..fst2].parse::<isize>().unwrap();

    let snd1 = line.chars().position(|ch| ch == 'Y').unwrap();

    let snd = line[snd1 + 2..].parse::<isize>().unwrap();

    (fst, snd)
}

type U = (usize, usize);

fn try_solve_single(s: (U, U, U)) -> Option<(usize, usize)> {
    let (ax, ay) = s.0;
    let (bx, by) = s.1;
    let (px, py) = s.2;

    for i in 0.. {
        for j in 0.. {
            if (ax * i + bx * j) == px && (ay * i + by * j) == py {
                return Some((i, j));
            }
            if j >= 100 {
                break;
            }
        }
        if i >= 100 {
            return None;
        }
    }
    None
}

fn solve_linear_system(
    a: (isize, isize),
    b: (isize, isize),
    c: (isize, isize),
) -> Option<(isize, isize)> {
    let det = a.0 * b.0 - a.1 * b.1;
    if det == 0 {
        return None;
    }

    let det_i = c.0 * b.1 - c.1 * b.0;
    let det_j = a.0 * c.1 - a.1 * c.0;

    if det_i % det != 0 || det_j % det != 0 {
        return None;
    }

    let i = det_i / det;
    let j = det_j / det;
    Some((i, j))
}

fn solve(input: &[String]) -> usize {
    let buttons = input
        .split(|line| line.is_empty())
        .map(|chunk| {
            let mut iter = chunk.iter();
            (
                split(iter.next().unwrap()),
                split(iter.next().unwrap()),
                split(iter.next().unwrap()),
            )
        })
        .collect::<Vec<(U, U, U)>>();

    let mut sum = 0;

    for s in buttons {
        if let Some((na, nb)) = try_solve_single(s) {
            sum += 3 * na + nb;
        }
    }

    sum
}

type I = (isize, isize);
fn solve_2(input: &[String]) -> usize {
    let buttons = input
        .split(|line| line.is_empty())
        .map(|chunk| {
            let mut iter = chunk.iter();
            (
                split2(iter.next().unwrap()),
                split2(iter.next().unwrap()),
                split2(iter.next().unwrap()),
            )
        })
        .collect::<Vec<(I, I, I)>>();

    let mut sum = 0;

    for s in buttons {
        if let Some((na, nb)) = solve_linear_system(s.0, s.1, s.2) {
            sum += 3 * na + nb;
        }
    }

    sum as usize
}

fn task_one(input: &[String]) -> usize {
    solve(input)
}

fn task_two(input: &[String]) -> usize {
    solve_2(input)
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
