fn manhatten_distance(n1: (usize, usize), n2: (usize, usize)) -> usize {
    n1.0.abs_diff(n2.0) + n1.1.abs_diff(n2.1)
}

fn solve<const EXP: usize>(input: &[String]) -> usize {
    let vec = input
        .iter()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let mut x_expansion = Vec::new();
    let mut y_expansion = Vec::new();

    for x in 0..vec[0].len() {
        if (0..vec.len()).all(|y| vec[y][x] == b'.') {
            x_expansion.push(x);
        }
    }
    for y in 0..vec.len() {
        if (0..vec[0].len()).all(|x| vec[y][x] == b'.') {
            y_expansion.push(y);
        }
    }

    let mut nodes = Vec::new();
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if vec[y][x] == b'#' {
                let x_inc: usize = x_expansion.iter().filter(|_x| x > **_x).count();
                let y_inc: usize = y_expansion.iter().filter(|_y| y > **_y).count();

                nodes.push((y + (y_inc * (EXP - 1)), x + (x_inc * (EXP - 1))));
            }
        }
    }

    let mut sum = 0;
    for i in 0..nodes.len() {
        for j in i + 1..nodes.len() {
            sum += manhatten_distance(nodes[i], nodes[j]);
        }
    }
    sum
}

fn task_one(input: &[String]) -> usize {
    solve::<2>(input)
}

fn task_two(input: &[String]) -> usize {
    solve::<1_000_000>(input)
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
