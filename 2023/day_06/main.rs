struct Data {
    time: Vec<usize>,
    dist: Vec<usize>,
}

fn parse(input: &[String]) -> Data {
    let time = input[0][5..]
        .split_ascii_whitespace()
        .map(|w| w.parse::<usize>().unwrap())
        .collect::<Vec<_>>();
    let dist = input[1][9..]
        .split_ascii_whitespace()
        .map(|w| w.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    Data { time, dist }
}

fn task_one(input: &[String]) -> usize {
    let data = parse(input);

    let mut sum = 1;
    for (time, dist) in data.time.into_iter().zip(data.dist.into_iter()) {
        let mut vec = Vec::new();

        for i in 0..time {
            vec.push(i * (time - i));
        }

        let ways = vec.into_iter().filter(|n| *n > dist).count();

        if ways > 0 {
            sum *= ways;
        }
    }
    sum
}

fn task_two(input: &[String]) -> usize {
    let data = parse(input);

    let parse = |vec: Vec<usize>| {
        let mut ch = String::new();
        for n in vec {
            ch.push_str(&n.to_string());
        }
        ch.parse::<usize>().unwrap()
    };

    let time = parse(data.time);
    let dist = parse(data.dist);

    let mut sum = 1;
    let mut vec = Vec::new();

    for i in 0..time {
        vec.push(i * (time - i));
    }

    let ways = vec.into_iter().filter(|n| *n > dist).count();

    if ways > 0 {
        sum *= ways;
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
