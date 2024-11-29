fn parse(input: &[String]) -> impl Iterator<Item = Vec<isize>> + '_ {
    input.iter().map(|line| {
        line.split_ascii_whitespace()
            .map(|tok| tok.parse::<isize>().unwrap())
            .collect()
    })
}

fn extrapolate(row: &[isize]) -> isize {
    if row.iter().all(|x| *x == 0) {
        return 0;
    }

    let new = row
        .windows(2)
        .map(|arr| arr[1] - arr[0])
        .collect::<Vec<_>>();

    let next = extrapolate(&new);
    return next + row.last().unwrap();
}

fn task_one(input: &[String]) -> isize {
    parse(input).map(|row| extrapolate(&row)).sum()
}

fn task_two(input: &[String]) -> isize {
    parse(input)
        .map(|mut row| {
            row.reverse();
            extrapolate(&row)
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
