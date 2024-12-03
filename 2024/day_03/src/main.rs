use regex::Regex;

fn mul(s: regex::Match<'_>) -> usize {
    let s = s.as_str();
    let s = s.replace("mul(", "");
    let s = s.replace(')', "");
    let (fst, snd) = s.split_once(',').unwrap();
    fst.parse::<usize>().unwrap() * snd.parse::<usize>().unwrap()
}

const MUL: &str = r"(mul\((\d+),(\d+)\))";

fn task_one(input: &[String]) -> usize {
    let re = Regex::new(MUL).unwrap();
    let mut sum = 0;
    for line in input {
        for cap in re.find_iter(line) {
            sum += mul(cap);
        }
    }
    sum
}

const DO: &str = r"do\(\)";
const DONT: &str = r"don't\(\)";

fn task_two(input: &[String]) -> usize {
    let re = Regex::new(&format!("{MUL}|{DO}|{DONT}")).unwrap();
    let mut sum = 0;
    let mut r#do = true;
    for line in input {
        for cap in re.find_iter(line) {
            match cap.as_str() {
                "do()" => r#do = true,
                "don't()" => r#do = false,
                _ if r#do => sum += mul(cap),
                _ => {}
            };
        }
    }
    sum
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
