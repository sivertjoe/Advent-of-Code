fn transform(input: &[String]) -> impl Iterator<Item = Vec<usize>> + '_ {
    input.iter().map(|line| {
        line.split_whitespace()
            .map(|n| n.parse::<usize>().unwrap())
            .collect()
    })
}

fn is_safe(nums: &[usize]) -> bool {
    let all_increasing_or_decreasing =
        || nums.windows(2).all(|w| w[0] > w[1]) || nums.windows(2).all(|w| w[0] < w[1]);

    let small_delta = || {
        nums.windows(2)
            .all(|w| (1..=3).contains(&w[0].abs_diff(w[1])))
    };

    all_increasing_or_decreasing() && small_delta()
}

fn is_safe2(nums: Vec<usize>) -> bool {
    let mut nums = nums;
    for i in 0..nums.len() {
        let elem = nums.remove(i);
        if is_safe(&nums) {
            return true;
        }
        nums.insert(i, elem);
    }
    false
}

fn task_one(input: &[String]) -> usize {
    transform(input).into_iter().filter(|v| is_safe(v)).count()
}

fn task_two(input: &[String]) -> usize {
    let mut count = 0;
    for v in transform(input) {
        if is_safe2(v) {
            count += 1;
        }
    }
    count
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
