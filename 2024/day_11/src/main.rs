use std::collections::*;

#[derive(Clone, Copy)]
struct Stone {
    value: u32,
    left: Option<u32>,
    right: Option<u32>,
}

fn split_number(num: u32) -> (u32, u32) {
    let num_str = num.to_string();
    let len = num_str.len();

    let mid = len / 2;

    let first_half = &num_str[..mid];
    let second_half = &num_str[mid..];

    let left = first_half.parse::<u32>().unwrap();
    let right = second_half.parse::<u32>().unwrap();

    (left, right)
}
fn task_one(input: &[String]) -> u32 {
    let mut vec = input[0]
        .split_whitespace()
        .map(|num| num.parse::<u32>().unwrap())
        .collect::<Vec<_>>();

    for _ in 0..25 {
        let mut new = Vec::new();
        for i in 0..vec.len() {
            let elem = vec[i];
            match elem {
                0 => new.push(1),
                n if n.to_string().len() % 2 == 0 => {
                    let (fst, snd) = split_number(n);
                    new.push(fst);
                    new.push(snd);
                }
                n => {
                    new.push(2024 * n);
                }
            }
        }
        vec = new;
    }

    vec.len() as u32
}

fn task_two(input: &[String]) -> usize {
    let mut vec = input[0]
        .split_whitespace()
        .map(|num| num.parse::<u32>().unwrap())
        .collect::<Vec<_>>();

    let mut new = Vec::new();
    for n in 0..75 {
        for i in 0..vec.len() {
            let elem = vec[i];
            match elem {
                0 => new.push(1),
                n if n.to_string().len() % 2 == 0 => {
                    let (fst, snd) = split_number(n);
                    new.push(fst);
                    new.push(snd);
                }
                n => {
                    new.push(2024 * n);
                }
            }
        }
        std::mem::swap(&mut vec, &mut new);
        new.clear();
    }

    vec.len()
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
