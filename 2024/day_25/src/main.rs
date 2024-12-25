fn parse(input: &[String]) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let mut locks = Vec::new();
    let mut keys = Vec::new();

    for chunk in input.split(|line| line.is_empty()) {
        let temp = chunk
            .iter()
            .map(|line| line.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();

        let mut count = Vec::new();
        for x in 0..5 {
            let mut local = 0;
            for y in 0..7 {
                if temp[y][x] == '#' {
                    local += 1;
                }
            }
            count.push(local - 1);
        }
        if temp[0][0] == '#' {
            locks.push(count);
        } else {
            keys.push(count);
        }
    }

    (locks, keys)
}

fn task_one(input: &[String]) -> usize {
    let (locks, keys) = parse(input);

    keys.iter()
        .flat_map(|key| {
            locks.iter().filter(|lock| {
                key.iter()
                    .zip(*lock)
                    .map(|(k1, k2)| *k1 + *k2)
                    .all(|n| n <= 5)
            })
        })
        .count()
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
