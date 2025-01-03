use std::collections::*;

fn parse(input: &[String]) -> (Vec<Vec<usize>>, HashMap<usize, Vec<usize>>) {
    let num = |s: &str| s.parse::<usize>().unwrap();
    let mut iter = input.split(String::is_empty);

    let mut map: HashMap<usize, Vec<usize>> = HashMap::new();
    for line in iter.next().unwrap() {
        let (k, v) = line.split_once('|').map(|(k, v)| (num(k), num(v))).unwrap();
        map.entry(k).or_default().push(v);
    }

    let mut vec = Vec::new();
    for line in iter.next().unwrap() {
        let update = line.split(',').map(num).collect();
        vec.push(update);
    }

    (vec, map)
}

fn task_one(input: &[String]) -> usize {
    let (pages, rules) = parse(input);

    pages
        .into_iter()
        .filter(|page| page.is_sorted_by(|a, b| rules[a].contains(b)))
        .map(|page| page[page.len() / 2])
        .sum()
}

fn task_two(input: &[String]) -> usize {
    use std::cmp::Ordering;
    let (pages, rules) = parse(input);
    pages
        .into_iter()
        .filter(|page| !page.is_sorted_by(|a, b| rules[a].contains(b)))
        .map(|mut page| {
            page.sort_by(|a, b| {
                if rules[a].contains(b) {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            });
            page[page.len() / 2]
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
