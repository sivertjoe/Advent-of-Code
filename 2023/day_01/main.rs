fn sum_calibration_values(input: &[String], part_two: bool) -> usize {
    input
        .iter()
        .map(|line| {
            let mut nums = get_numbers(line, part_two);
            let min = nums.next().unwrap();
            let max = nums.last().unwrap_or(min);
            min * 10 + max
        })
        .sum()
}

fn get_numbers(line: &str, part_two: bool) -> impl Iterator<Item = usize> + '_ {
    const NUMS: [&str; 9] = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    let bytes = line.as_bytes();

    (0..line.len()).flat_map(move |i| match bytes[i] {
        b @ b'0'..=b'9' => Some((b - b'0') as usize),
        _ if part_two => NUMS
            .iter()
            .enumerate()
            .find_map(|(num, text)| (line[i..].starts_with(text)).then_some(num + 1)),
        _ => None,
    })
}

fn task_one(input: &[String]) -> usize {
    sum_calibration_values(input, false)
}

fn task_two(input: &[String]) -> usize {
    sum_calibration_values(input, true)
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
