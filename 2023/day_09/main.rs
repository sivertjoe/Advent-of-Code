fn parse(input: &[String]) -> Vec<Vec<isize>> {
    input
        .iter()
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|tok| tok.parse::<isize>().unwrap())
                .collect()
        })
        .collect()
}

fn extrapolate(row: Vec<isize>) -> isize {
    let mut rows = Vec::new();
    rows.push(row);

    while !rows.last().unwrap().iter().all(|v| *v == 0) {
        let last = rows.last().unwrap();
        let new = last
            .windows(2)
            .map(|arr| arr[1] - arr[0])
            .collect::<Vec<_>>();
        rows.push(new);
    }
    rows.last_mut().unwrap().push(0);

    for i in (0..rows.len()).rev().skip(1) {
        let left = rows[i][rows[i].len() - 1];
        let below = rows[i + 1].last().copied().unwrap();
        *rows[i].last_mut().unwrap() = left + below;
    }

    rows[0].last().copied().unwrap()
}

fn extrapolate2(row: Vec<isize>) -> isize {
    let mut rows = Vec::new();
    rows.push(row);

    while !rows.last().unwrap().iter().all(|v| *v == 0) {
        let last = rows.last().unwrap();
        let new = last
            .windows(2)
            .map(|arr| arr[1] - arr[0])
            .collect::<Vec<_>>();
        rows.push(new);
    }
    rows.last_mut().unwrap().push(0);

    for i in (0..rows.len()).rev().skip(1) {
        let right = rows[i][0];
        let below = rows[i + 1][0];
        *rows[i].first_mut().unwrap() = right - below;
    }

    rows[0].first().copied().unwrap()
}

fn task_one(input: &[String]) -> isize {
    parse(input).into_iter().map(extrapolate).sum()
}

fn task_two(input: &[String]) -> isize {
    parse(input).into_iter().map(extrapolate2).sum()
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
