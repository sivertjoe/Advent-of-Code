use std::collections::*;

fn transform(input: &[String]) -> impl Iterator<Item = (usize, usize)> + '_ {
    input.iter().map(|line| {
        let mut spl = line.split_whitespace();
        let fst = spl.next().unwrap().parse::<usize>().unwrap();
        let snd = spl.next().unwrap().parse::<usize>().unwrap();
        (fst, snd)
    })
}

fn task_one(input: &[String]) -> usize {
    let mut vec1 = Vec::new();
    let mut vec2 = Vec::new();

    for (fst, snd) in transform(input) {
        vec1.push(fst);
        vec2.push(snd);
    }

    vec1.sort();
    vec2.sort();

    vec1.into_iter()
        .zip(vec2)
        .map(|(a, b)| a.max(b) - a.min(b))
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let mut vec = Vec::new();
    let mut map = HashMap::new();

    for (fst, snd) in transform(input) {
        vec.push(fst);
        *map.entry(snd).or_insert(0) += 1;
    }

    vec.into_iter()
        .filter_map(|elem| map.get(&elem).map(|occ| elem * occ))
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
