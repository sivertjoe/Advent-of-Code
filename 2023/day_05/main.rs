use itertools::Itertools;
use std::collections::*;

#[derive(Debug)]
struct Map {
    dst: usize,
    src: usize,
    range: usize,
}

#[derive(Debug)]
struct Data {
    seeds: Vec<usize>,
    maps: Vec<Vec<Map>>,
}

fn parse(input: &[String]) -> Data {
    let mut maps = Vec::new();
    let mut v = Vec::new();
    for inp in input.split(|line| line.is_empty()) {
        if inp[0].starts_with("seeds") {
            v = inp[0][6..]
                .split_ascii_whitespace()
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
        } else {
            let next = inp
                .iter()
                .skip(1)
                .map(|line| {
                    let mut line = line
                        .split_ascii_whitespace()
                        .map(|w| w.parse::<usize>().unwrap());
                    let dst = line.next().unwrap();
                    let src = line.next().unwrap();
                    let range = line.next().unwrap();

                    Map { dst, src, range }
                })
                .collect::<Vec<_>>();
            maps.push(next);
        }
    }
    Data { seeds: v, maps }
}

fn task_one(input: &[String]) -> usize {
    let data = parse(input);
    let mut nums = data.seeds.clone();

    for map in data.maps {
        for val in nums.iter_mut() {
            if let Some(next) = map.iter().find(|l| (l.src..l.src + l.range).contains(&val)) {
                let diff = *val - next.src;
                let nv = next.dst + diff;
                *val = nv;
            }
        }
    }
    nums.into_iter().min().unwrap()
}

fn thread_exec<T, U, I, F, R>(iter: I, f: F) -> R
where
    F: Fn(T) -> U + Send + Clone + Copy,
    R: FromIterator<U>,
    U: Send,
    T: Send,
    I: IntoIterator<Item = T>,
{
    // Collecting the JoinHandles are very important to actually spawn the threads.
    // Removing the collect results in sequential execution
    #[allow(clippy::needless_collect)]
    std::thread::scope(|s| {
        iter.into_iter()
            .map(|v| s.spawn(move || f(v)))
            .collect::<Vec<_>>()
            .into_iter()
            .map(|h| h.join().unwrap())
            .collect::<R>()
    })
}

fn task_two(input: &[String]) -> usize {
    let data = parse(input);
    let mut min = usize::MAX;

    let mut calc = |(start, range): (usize, usize)| {
        let mut min = usize::MAX;
        for num in (start..start + range) {
            let mut num = num;
            for map in data.maps.iter() {
                if let Some(next) = map.iter().find(|l| (l.src..l.src + l.range).contains(&num)) {
                    let diff = num - next.src;
                    let nv = next.dst + diff;
                    num = nv;
                }
            }
            min = min.min(num);
        }
        dbg!(min)
    };
    let res: Vec<usize> = thread_exec(data.seeds.iter().copied().tuples(), calc);

    res.into_iter().min().unwrap()
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
