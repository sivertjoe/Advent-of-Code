use std::collections::*;

fn neighbors(map: &[Vec<u8>], (y, x): (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let my = map.len() as isize;
    let mx = map[0].len() as isize;
    let x = x as isize;
    let y = y as isize;
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        .into_iter()
        .filter(move |(x, y)| *x >= 0 && *x < mx && *y >= 0 && *y < my)
        .map(|(x, y)| (x as usize, y as usize))
}

fn solve(input: &[String], unique_only: bool) -> usize {
    let map: Vec<Vec<_>> = input.iter().map(|line| line.bytes().collect()).collect();

    let mut scores: HashMap<(usize, usize), usize> = HashMap::new();

    let mut starts = Vec::new();
    for (y, line) in map.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            if *ch == b'0' {
                starts.push((y, x));
            }
        }
    }

    for start in starts {
        let mut vec = VecDeque::new();
        let mut seen = HashSet::new();
        vec.push_back((0, start));
        seen.insert(start);

        while let Some((cost, pos)) = vec.pop_front() {
            let elem = map[pos.0][pos.1];
            if elem == b'9' {
                *scores.entry(start).or_default() += 1;
                continue;
            }

            for neighbor in neighbors(&map, pos).filter(|next| map[next.0][next.1] == elem + 1) {
                if unique_only {
                    if seen.insert(neighbor) {
                        vec.push_back((cost + 1, neighbor));
                    }
                } else {
                    vec.push_back((cost + 1, neighbor));
                }
            }
        }
    }
    scores.into_values().sum()
}

fn task_one(input: &[String]) -> usize {
    solve(input, true)
}

fn task_two(input: &[String]) -> usize {
    solve(input, false)
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
