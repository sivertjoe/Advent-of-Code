fn rec<O>(total: usize, parts: &[usize], running: usize, opers: &[O]) -> bool
where
    O: Fn(usize, usize) -> usize,
{
    match parts {
        _ if running > total => false,
        [] => total == running,
        [head, rest @ ..] => opers
            .iter()
            .any(|op| rec(total, rest, op(running, *head), opers)),
    }
}

fn solve<O>(input: &[String], opers: &[O]) -> usize
where
    O: Fn(usize, usize) -> usize + Sync,
{
    use rayon::prelude::*;
    let num = |s: &str| s.parse::<usize>().unwrap();
    input
        .par_iter()
        .map(|line| {
            let (fst, snd) = line.split_once(": ").unwrap();
            let fst = num(fst);
            let snd = snd.split_whitespace().map(num).collect::<Vec<_>>();

            (fst, snd)
        })
        .filter(|(total, parts)| rec(*total, &parts[1..], parts[0], opers))
        .map(|(total, _parts)| total)
        .sum()
}

fn task_one(input: &[String]) -> usize {
    let opers = [|a, b| a + b, |a, b| a * b];
    solve(input, &opers)
}

fn task_two(input: &[String]) -> usize {
    fn concat(a: usize, b: usize) -> usize {
        a * 10usize.pow(b.ilog(10usize) + 1) + b
    }
    #[rustfmt::skip]
    let opers = [
        |a, b| a + b,
        |a, b| a * b,
        |a, b: usize| concat(a, b),
    ];
    solve(input, &opers)
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
