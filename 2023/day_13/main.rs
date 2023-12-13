use std::collections::*;

struct Pattern(Vec<Vec<u8>>);

fn parse(input: &[String]) -> Vec<Pattern> {
    input
        .split(|line| line.is_empty())
        .into_iter()
        .map(|lines| {
            Pattern(
                lines
                    .into_iter()
                    .map(|line| line.as_bytes().to_vec())
                    .collect::<Vec<_>>(),
            )
        })
        .collect()
}

fn _print(p: &Pattern) {
    for row in &p.0 {
        for ch in row {
            print!("{}", *ch as char);
        }
        println!();
    }
    println!();
}

fn summarize(pat: &Pattern) -> Option<usize> {
    let cmp_cols = |i: usize, j: usize| {
        for y in 0..pat.0.len() {
            if pat.0[y][i] != pat.0[y][j] {
                return false;
            }
        }
        true
    };
    let check_horizontal = |i: usize, j: usize| {
        for (i, j) in (0..=i).rev().zip(j..pat.0[0].len()) {
            if !cmp_cols(i, j) {
                return None;
            }
        }
        return Some(i);
    };

    for i in 0..pat.0[0].len() - 1 {
        if let Some(v) = check_horizontal(i, i + 1) {
            return Some(v + 1);
        }
    }

    let check_vertical = |i: usize, j: usize| {
        for (i, j) in (0..=i).rev().zip(j..pat.0.len()) {
            if pat.0[i] != pat.0[j] {
                return None;
            }
        }
        return Some(i);
    };

    for i in 0..pat.0.len() - 1 {
        if let Some(v) = check_vertical(i, i + 1) {
            return Some(100 * (v + 1));
        }
    }

    None
}

#[derive(PartialEq, Eq, Debug)]
enum Line {
    Horizontal((usize, usize)),
    Vertical((usize, usize)),
}

impl Line {
    fn sum(&self) -> usize {
        match self {
            Self::Horizontal((i, j)) => i + 1,
            Self::Vertical((i, j)) => (i + 1) * 100,
        }
    }
}

fn summarize2(pat: &Pattern) -> Option<Line> {
    let cmp_cols = |i: usize, j: usize| {
        for y in 0..pat.0.len() {
            if pat.0[y][i] != pat.0[y][j] {
                return false;
            }
        }
        true
    };
    let check_horizontal = |i: usize, j: usize| {
        for (i, j) in (0..=i).rev().zip(j..pat.0[0].len()) {
            if !cmp_cols(i, j) {
                return None;
            }
        }
        return Some(Line::Horizontal((i, j)));
    };

    for i in 0..pat.0[0].len() - 1 {
        if let Some(v) = check_horizontal(i, i + 1) {
            return Some(v);
        }
    }

    let check_vertical = |i: usize, j: usize| {
        for (i, j) in (0..=i).rev().zip(j..pat.0.len()) {
            if pat.0[i] != pat.0[j] {
                return None;
            }
        }
        return Some(Line::Vertical((i, j)));
    };

    for i in 0..pat.0.len() - 1 {
        if let Some(v) = check_vertical(i, i + 1) {
            return Some(v);
        }
    }

    None
}

fn task_one(input: &[String]) -> usize {
    let patterns = parse(input);

    patterns.iter().flat_map(summarize).sum()
}

fn task_two(input: &[String]) -> usize {
    let patterns = parse(input);

    let opp = |ch: u8| if ch == b'#' { b'.' } else { b'#' };

    patterns
        .into_iter()
        .map(|mut pat| {
            let old = summarize2(&pat).unwrap();

            for y in 0..pat.0.len() {
                for x in 0..pat.0[0].len() {
                    pat.0[y][x] = opp(pat.0[y][x]);
                    if let Some(v) = summarize2(&pat) {
                        if old != v {
                            return v.sum();
                        }
                    }
                    pat.0[y][x] = opp(pat.0[y][x]);
                }
            }
            unreachable!()
        })
        .sum()
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
