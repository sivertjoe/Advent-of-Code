use rayon::prelude::*;
use std::collections::*;

fn parse(input: &[String]) -> (Vec<String>, Vec<String>) {
    let towels = input[0].split(", ").map(|t| t.to_string()).collect();
    let designs = input[2..].iter().map(|line| line.to_string()).collect();

    (towels, designs)
}

fn count_number_of_ways(design: &str, towels: &[String]) -> usize {
    let mut cache = HashMap::new();
    cache.insert("".to_string(), 1);
    for i in (0..design.len()).rev() {
        let s = &design[i..];
        for towel in towels {
            if s.starts_with(towel) {
                let rest = &s[towel.len()..];
                let count = *cache.get(rest).unwrap_or(&0);
                *cache.entry(s.to_string()).or_default() += count;
            }
        }
    }
    *cache.get(design).unwrap_or(&0)
}

fn task_one(input: &[String]) -> usize {
    let (towels, designs) = parse(input);

    designs
        .into_par_iter()
        .filter(|design| count_number_of_ways(&design, &towels) > 0)
        .count()
}

fn task_two(input: &[String]) -> usize {
    let (towels, designs) = parse(input);

    designs
        .into_par_iter()
        .map(|design| count_number_of_ways(&design, &towels))
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
