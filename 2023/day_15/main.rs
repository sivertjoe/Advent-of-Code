use std::collections::*;

fn hash(s: &str) -> u32 {
    s.trim()
        .bytes()
        .fold(0u32, |acc, b| ((acc + (b as u32)) * 17) % 256)
}

fn task_one(input: &str) -> u32 {
    input.split(',').map(hash).sum()
}

fn task_two(input: &str) -> usize {
    let mut arr: Vec<Vec<(&str, usize)>> = vec![vec![]; 256];

    for s in input.split(',') {
        let s = s.trim();
        if s.contains('=') {
            let (fst, snd) = s.split_once('=').unwrap();
            let idx = hash(fst) as usize;
            let snd = snd.parse::<usize>().unwrap();

            if let Some(lense) = arr[idx].iter_mut().find(|(s, _)| *s == fst) {
                lense.1 = snd;
            } else {
                arr[idx].push((fst, snd));
            }
        } else {
            let fst = &s[..s.len() - 1];
            let idx = hash(fst) as usize;

            arr[idx].retain(|(s, _)| *s != fst);
        }
    }

    let mut sum = 0;
    for (i, b) in arr.into_iter().enumerate() {
        for (j, (_, lense)) in b.into_iter().enumerate() {
            sum += (i + 1) * (j + 1) * lense;
        }
    }
    sum
}

fn main() {
    let input = std::fs::read_to_string(get_input_file()).unwrap();
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
