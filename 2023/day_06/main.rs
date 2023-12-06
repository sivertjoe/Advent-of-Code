fn parse(input: &[String]) -> (Vec<usize>, Vec<usize>) {
    let parse = |s: &str| {
        s.split_ascii_whitespace()
            .map(|w| w.parse::<usize>().unwrap())
            .collect::<Vec<_>>()
    };
    let time = parse(&input[0][5..]);
    let dist = parse(&input[1][9..]);

    (time, dist)
}

fn get_number_of_winning_ways((time, dist): (usize, usize)) -> usize {
    let time = time as f64;
    let dist = dist as f64;

    let discriminant = (time * time - (4.0 * (-dist) * -1.0)).sqrt();

    let lower = (-time + discriminant) / -2.0;
    let higher = (-time - discriminant) / -2.0;

    if lower.fract() == 0.0 {
        (higher - lower) as usize - 1
    } else {
        higher.floor() as usize - lower.ceil() as usize + 1
    }
}

fn task_one(input: &[String]) -> usize {
    let (time, dist) = parse(input);

    time.into_iter()
        .zip(dist)
        .map(get_number_of_winning_ways)
        .product()
}

fn task_two(input: &[String]) -> usize {
    let (time, dist) = parse(input);

    let parse = |vec: Vec<usize>| {
        vec.into_iter()
            .map(|num| num.to_string())
            .collect::<String>()
            .parse()
            .unwrap()
    };

    let time = parse(time);
    let dist = parse(dist);

    get_number_of_winning_ways((time, dist))
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
