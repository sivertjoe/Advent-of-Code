use itertools::Itertools;

fn sum_calibration_values<'a, F, I>(input: &'a [String], parse_func: F) -> u32
where
    F: Fn(&'a String) -> I,
    I: Iterator<Item = (usize, u32)>,
{
    input
        .iter()
        .map(|line| {
            parse_func(line)
                .minmax_by_key(|(idx, _)| *idx)
                .into_option()
                .map(|(min, max)| min.1 * 10 + max.1)
                .unwrap()
        })
        .sum()
}

fn parse_char_digits(line: &String) -> impl Iterator<Item = (usize, u32)> + '_ {
    line.chars()
        .enumerate()
        .filter_map(|(idx, ch)| ch.to_digit(10).map(|d| (idx, d)))
}

fn parse_text_digits(line: &String) -> impl Iterator<Item = (usize, u32)> + '_ {
    [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ]
    .iter()
    .enumerate()
    .flat_map(|(i, text_num)| {
        line.match_indices(text_num)
            .map(move |(idx, _)| (idx, i as u32 + 1))
    })
}

fn task_one(input: &[String]) -> u32 {
    sum_calibration_values(input, parse_char_digits)
}

fn task_two(input: &[String]) -> u32 {
    sum_calibration_values(input, |line| {
        parse_char_digits(line).chain(parse_text_digits(line))
    })
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
    let elapsed = t.elapsed().as_millis();

    match task {
        Task::One => {
            println!("({}ms)\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}ms)\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
