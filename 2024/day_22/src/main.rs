use rayon::prelude::*;
use std::collections::*;

fn mix(secret: usize, num: usize) -> usize {
    secret ^ num
}

fn prune(secret: usize) -> usize {
    secret % 16777216
}

fn next(secret: usize) -> usize {
    let temp = secret * 64;
    let secret = prune(mix(secret, temp));
    let temp = secret / 32;
    let secret = prune(mix(secret, temp));
    let temp = secret * 2048;
    let secret = prune(mix(secret, temp));
    secret
}

fn task_one(input: &[String]) -> usize {
    input
        .iter()
        .map(|line| {
            let mut num = line.parse::<usize>().unwrap();
            for _ in 0..2000 {
                num = next(num);
            }
            num
        })
        .sum()
}

fn num_bananas(map: &HashMap<A, usize>, seq: A) -> usize {
    *map.get(&seq).unwrap_or(&0)
}

type A = [isize; 4];
fn calc_sum(nums: &[HashMap<A, usize>], seq: A) -> usize {
    nums.into_iter().map(|num| num_bananas(&num, seq)).sum()
}

fn task_two(input: &[String]) -> usize {
    let nums = input
        .iter()
        .map(|line| {
            let mut vec = Vec::new();
            let mut num = line.parse::<usize>().unwrap();
            for _ in 0..2000 {
                let n = next(num);

                vec.push((n % 10, (n % 10) as isize - (num % 10) as isize));
                num = n;
            }

            let mut map = HashMap::with_capacity(vec.len() * 4);
            for w in vec.windows(4) {
                let seq = [w[0].1, w[1].1, w[2].1, w[3].1];
                let num = w[3].0;
                if !map.contains_key(&seq) {
                    map.insert(seq, num);
                }
            }
            map
        })
        .collect::<Vec<_>>();

    let max = (-9..=9)
        .into_par_iter()
        .map(|w| {
            let mut max = 0;
            for z in -9..=9 {
                for y in -9..=9 {
                    for x in -9..=9 {
                        let seq = [w, z, y, x];
                        let res = calc_sum(&nums, seq);
                        max = max.max(res);
                    }
                }
            }
            max
        })
        .max()
        .unwrap();
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
