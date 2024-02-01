use std::collections::*;

type Map<K, V> = HashMap<K, V, std::hash::BuildHasherDefault<fxhash::FxHasher>>;
//type Set<V> = HashSet<V, std::hash::BuildHasherDefault<fxhash::FxHasher>>;

fn task_one(input: &[String]) -> usize {
    let mut v = HashSet::new();
    let mut e = HashSet::new();

    for line in input {
        let (fst, snd) = line.split_once(": ").unwrap();
        v.insert(fst.to_string());

        for snd in snd.split_ascii_whitespace() {
            v.insert(snd.to_string());
            e.insert((fst.to_string(), snd.to_string()));
        }
    }

    let mut rng = rand::thread_rng();
    use rand::prelude::*;

    let mut subsets: Vec<HashSet<String>> = Vec::new();
    subsets = v
        .iter()
        .map(|v| HashSet::from_iter(std::iter::once(v.to_string())))
        .collect();

    loop {
        let mut rng = rand::thread_rng();
        let (v, w) = e.choose(&mut rng).unwrap();

        let s1 = ss(&subsets, &v);
        let s2 = ss(&subsets, &w);

        if s1 != s2 {
            let s1_index = subsets.iter().position(|s| s.contains(&v)).unwrap();
            let s2_index = subsets.iter().position(|s| s.contains(&w)).unwrap();

            subsets[s1_index].extend(subsets[s2_index].drain());
            subsets.remove(s2_index);
        }

        if subsets.len() <= 2 {
            break;
        }
    }

    subsets.iter().map(|s| s.len()).product()
}

fn ss(subsets: &[HashSet<String>], v: &str) -> HashSet<String> {
    subsets.iter().find(|s| s.contains(v)).cloned().unwrap()
}

fn explore(map: &Map<String, Vec<String>>, start: String) -> usize {
    let mut seen = HashSet::new();
    seen.insert(start.as_str());

    let mut stack = vec![start.as_str()];
    while let Some(curr) = stack.pop() {
        if let Some(ns) = map.get(curr) {
            for n in ns {
                if seen.insert(n.as_str()) {
                    stack.push(n.as_str());
                }
            }
        }
    }

    seen.len()
}

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
