fn next(vec: &[Vec<char>], pos: (usize, usize), inc: (isize, isize)) -> Option<(usize, usize)> {
    let y_max = vec.len();
    let x_max = vec[0].len();

    let next = (pos.0 as isize + inc.0, pos.1 as isize + inc.1);

    if next.0 >= 0 && next.0 < y_max as isize && next.1 >= 0 && next.1 < x_max as isize {
        Some((next.0 as usize, next.1 as usize))
    } else {
        None
    }
}

fn check(vec: &[Vec<char>], pos: (usize, usize), inc: (isize, isize)) -> bool {
    let mut pos = pos;
    for ch in ['M', 'A', 'S'].into_iter() {
        let Some(new) = next(vec, pos, inc) else {
            return false;
        };
        if vec[new.0][new.1] != ch {
            return false;
        }
        pos = new;
    }
    true
}

fn check2(vec: &[Vec<char>], y: usize, x: usize) -> bool {
    let mut s = String::with_capacity(3);
    s.push(vec[y][x]);
    s.push(vec[y + 1][x + 1]);
    s.push(vec[y + 2][x + 2]);

    if s != "MAS" && s != "SAM" {
        return false;
    }

    s.clear();
    s.push(vec[y][x + 2]);
    s.push(vec[y + 1][x + 1]);
    s.push(vec[y + 2][x]);
    if s != "MAS" && s != "SAM" {
        return false;
    }

    true
}

fn task_one(input: &[String]) -> usize {
    let vec: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();

    let xs: Vec<_> = vec
        .iter()
        .enumerate()
        .flat_map(|(y, vec)| {
            vec.iter()
                .enumerate()
                .filter_map(move |(x, ch)| (*ch == 'X').then_some((y, x)))
        })
        .collect();

    let mut sum = 0;
    let dirs = [
        (0, 1),
        (0, -1),
        (1, 0),
        (-1, 0),
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
    ];
    for x in xs {
        sum += dirs.iter().filter(|dir| check(&vec, x, **dir)).count();
    }

    sum
}

fn task_two(input: &[String]) -> usize {
    let vec: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
    let mut sum = 0;

    for y in 0..vec.len() - 2 {
        for x in 0..vec[0].len() - 2 {
            if check2(&vec, y, x) {
                sum += 1;
            }
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
