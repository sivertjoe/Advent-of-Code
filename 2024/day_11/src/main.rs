use std::collections::*;

fn split_number(num: usize) -> (usize, usize) {
    let num_digits = (num as f64).log10() as usize + 1;
    let half_digits = num_digits / 2;

    let divisor = 10_usize.pow(half_digits as u32);

    let left = num / divisor;
    let right = num % divisor;

    (left, right)
}

fn solve<const N: usize>(input: &[String]) -> usize {
    let mut map = input[0]
        .split_whitespace()
        .map(|num| (num.parse::<usize>().unwrap(), 1))
        .collect::<fxhash::FxHashMap<usize, usize>>();

    let is_even = |n: usize| (n.checked_ilog10().unwrap_or(0) + 1) % 2 == 0;

    let mut new = fxhash::FxHashMap::default();
    for _ in 0..N {
        new.clear();
        for (k, v) in map.iter() {
            match k {
                0 => *new.entry(1).or_default() += v,
                n if is_even(*n) => {
                    let (fst, snd) = split_number(*n);
                    *new.entry(fst).or_default() += v;
                    *new.entry(snd).or_default() += v;
                }
                n => {
                    let n = n * 2024;
                    *new.entry(n).or_default() += v;
                }
            }
        }
        std::mem::swap(&mut map, &mut new);
    }

    map.into_values().sum()
}

fn task_one(input: &[String]) -> usize {
    solve::<25>(input)
}

fn task_two(input: &[String]) -> usize {
    solve::<75>(input)
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
