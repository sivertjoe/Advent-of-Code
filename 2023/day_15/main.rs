use std::collections::*;

fn hash(s: &str) -> u32 {
    let mut curr = 0;
    for b in s.bytes() {
        curr += b as u32;
        curr *= 17;
        curr %= 256;
    }
    curr
}

fn task_one(_input: &[String]) -> u32 {
    let mut sum = 0;
    let file = std::fs::read_to_string(get_input_file()).unwrap();

    for s in file.split(',') {
        sum += hash(s.trim());
    }
    sum
}

fn task_two(input: &[String]) -> usize {
    let file = std::fs::read_to_string(get_input_file()).unwrap();

    let mut map: Vec<Vec<(String, usize)>> = vec![vec![]; 256];

    for s in file.split(',') {
        let s = s.trim();
        if s.contains('=') {
            let (fst, snd) = s.split_once('=').unwrap();
            let idx = hash(fst) as usize;
            let fst = fst.to_string();
            let snd = snd.parse::<usize>().unwrap();

            let mut b = &mut map[idx];
            if let Some(lense) = b.iter_mut().find(|(s, _)| *s == fst) {
                lense.1 = snd;
            } else {
                b.push((fst, snd));
            }
        } else {
            let (fst, snd) = s.split_once('-').unwrap();
            let idx = hash(fst) as usize;
            let fst = fst.to_string();
            //let snd = snd.parse::<usize>().unwrap();

            let mut b = &mut map[idx];
            if let Some(pos) = b.iter().position(|(s, _)| *s == fst) {
                b.remove(pos);
            }
        }
    }

    let mut sum = 0;
    for (i, b) in map.into_iter().enumerate() {
        for (j, lense) in b.into_iter().enumerate() {
            sum += (i + 1) * (j + 1) * lense.1;
        }
    }

    sum
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
