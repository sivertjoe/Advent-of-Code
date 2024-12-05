use std::collections::*;

fn parse(input: &[String]) -> (Vec<Vec<usize>>, HashMap<usize, Vec<usize>>) {
    let mut map: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut vec = Vec::new();
    let mut b = true;
    for line in input {
        if line.is_empty() {
            b = false;
            continue;
        }
        if b {
            let (a, b) = line.split_once('|').unwrap();

            let num = (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap());
            map.entry(num.0).or_default().push(num.1);
        } else {
            let nums = line
                .split(',')
                .map(|num| num.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            vec.push(nums);
        }
    }

    (vec, map)
}

fn verify_update(update: &[usize], map: &HashMap<usize, Vec<usize>>) -> bool {
    for (i, num) in update.iter().enumerate() {
        if let Some(elems) = map.get(num) {
            if update[..i].iter().any(|elem| elems.contains(elem)) {
                return false;
            }
        }
    }
    true
}

fn make_correct(vec: Vec<usize>, map: &HashMap<usize, Vec<usize>>) -> Vec<usize> {
    let mut indexed_counts = vec
        .iter()
        .enumerate()
        .map(|(i, n)| {
            let count = map
                .get(n)
                .map(|elems| elems.iter().filter(|elem| vec.contains(elem)).count())
                .unwrap_or(0);
            (i, count)
        })
        .collect::<Vec<_>>();

    use std::cmp::Reverse;
    indexed_counts.sort_by_key(|k| Reverse(k.1));

    indexed_counts.into_iter().map(|(i, _)| vec[i]).collect()
}

fn task_one(input: &[String]) -> usize {
    let (updates, rules) = parse(input);

    updates
        .into_iter()
        .filter(|update| verify_update(update, &rules))
        .map(|update| update[update.len() / 2])
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let (updates, rules) = parse(input);

    updates
        .into_iter()
        .filter(|update| !verify_update(update, &rules))
        .map(|update| make_correct(update, &rules))
        .map(|update| update[update.len() / 2])
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
