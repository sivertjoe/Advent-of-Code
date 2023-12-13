fn parse(input: &[String]) -> Vec<(Vec<u8>, Vec<usize>)> {
    input
        .iter()
        .map(|line| {
            let (fst, snd) = line.split_once(' ').unwrap();
            let fst = fst.bytes().collect::<Vec<_>>();
            let snd = snd
                .split(',')
                .map(|d| d.parse::<usize>().unwrap())
                .collect::<Vec<_>>();

            (fst, snd)
        })
        .collect()
}

fn arrangements((spring, nums): (Vec<u8>, Vec<usize>)) -> usize {
    let mut spring = spring;
    if let Some(idx) = spring.iter().rev().position(|elm| *elm != b'.') {
        spring.truncate(spring.len() - idx + 1)
    }
    spring.insert(0, b'.');

    let mut dp = vec![0; spring.len() + 1];

    for i in (0..spring.len()).take_while(|i| spring[*i] != b'#') {
        dp[i] = 1;
    }

    for num in nums {
        let mut count = 0;
        let mut new = vec![0; spring.len()+1];

        for (i, ch) in spring.iter().copied().enumerate() {
            if ch == b'.' {
                count = 0;
            } else {
                count += 1;
            }
            if ch != b'#' {
                new[i + 1] += new[i];
            }
            if count >= num && spring[i - num] != b'#' {
                new[i + 1] += dp[i - num];
            }
        }
        dp = new;
    }

    *dp.last().unwrap()
}

fn transform((recs, nums): (Vec<u8>, Vec<usize>)) -> (Vec<u8>, Vec<usize>) {
    let mut nrecs = Vec::with_capacity(recs.len() * 5);
    let mut nnums = Vec::with_capacity(nums.len() * 5);

    for _ in 0..5 {
        nrecs.extend(recs.clone());
        nrecs.push(b'?');
        nnums.extend(nums.clone());
    }
    let _ = nrecs.pop();

    (nrecs, nnums)
}

type T = (Vec<u8>, Vec<usize>);
fn solve<F: Fn(T) -> T>(input: &[String], f: F) -> usize {
    parse(input)
        .into_iter()
        .map(f)
        .map(arrangements)
        .sum()
}
fn task_one(input: &[String]) -> usize {
    use std::convert::identity;
    solve(input, identity)
}

fn task_two(input: &[String]) -> usize {
    solve(input, transform)
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
