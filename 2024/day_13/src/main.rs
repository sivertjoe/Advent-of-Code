use nalgebra::{Vector2, Matrix2};

fn split(line: &String) -> Vector2<f64> {
    let mut parts = line.split([',', ' ']);
    let x = parts.find(|s| s.starts_with('X')).unwrap()[2..].parse::<f64>().unwrap();
    let y = parts.find(|s| s.starts_with('Y')).unwrap()[2..].parse::<f64>().unwrap();

    Vector2::new(x, y)
}

fn try_get_whole(a: f64, b: f64) -> Option<(usize, usize)>
{
    let epsilon = 1e-3;
    if (a - a.round()).abs() < epsilon && (b - b.round()).abs() < epsilon {
        Some((a.round() as usize, b.round() as usize))
    } else {
        None
    }
}

fn solve(input: &[String], part_2: bool) -> usize {
    let parse = |chunk: &[String]| {
        let mut iter = chunk.iter().map(split);
        let a  = iter.next().unwrap();
        let b = iter.next().unwrap();
        let mut c = iter.next().unwrap();

        if part_2 {
            c[0] += 10000000000000.0;
            c[1] += 10000000000000.0;
        }

        let mat = Matrix2::new(a[0], b[0], a[1], b[1]);
        (mat, c)
    };

    let machines = input
        .split(|line| line.is_empty())
        .map(parse);

    let mut sum = 0;
    for (mat, c) in machines {
        if let Some((i, j)) = mat.try_inverse().map(|inv| inv * c).and_then(|sol| try_get_whole(sol[0],sol[1])) {
            if part_2 || i <= 100 && j <= 100 {
                sum += 3 * i + j;
            }
        }
    }
    sum
}

fn task_one(input: &[String]) -> usize {
    solve(input, false)
}

fn task_two(input: &[String]) -> usize {
    solve(input, true)
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
