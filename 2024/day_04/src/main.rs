use std::collections::*;

fn task_one(input: &[String]) -> usize {
    let vec: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();

    let mut sum = 0;
    let y_max = vec.len();
    let x_max = vec[0].len();

    // horizontal f/b
    for y in 0..y_max {
        sum += check(&vec, (y as _, 0), (0, 1));
        sum += check(&vec, (y as _, (x_max - 1) as isize), (0, -1));
    }

    // vertical f/b
    for x in 0..x_max {
        sum += check(&vec, (0, x as _), (1, 0));
        sum += check(&vec, ((y_max - 1) as isize, x as _), (-1, 0));
    }

    // diagonal
    for y in 0..y_max {
        sum += check(&vec, (y as _, 0), (1, -1));
        sum += check(&vec, (y as _, 0), (1, 1));

        sum += check(&vec, ((y_max - 1) as _, 0), (-1, 1));
        sum += check(&vec, ((y_max - 1) as _, 0), (-1, -1));
    }

    sum
}

fn next(vec: &Vec<Vec<char>>, pos: (isize, isize), inc: (isize, isize)) -> Option<(usize, usize)> {
    let y_max = vec.len();
    let x_max = vec[0].len();

    let next = (pos.0 + inc.0, pos.1 + inc.1);

    if next.0 >= 0 && (next.0 as usize) < y_max && next.1 >= 0 && (next.1 as usize) < x_max {
        Some((next.0 as usize, next.1 as usize))
    } else {
        None
    }
}

fn check(vec: &Vec<Vec<char>>, pos: (isize, isize), inc: (isize, isize)) -> usize {
    let mut total = 0;

    let word = vec!['X', 'M', 'A', 'S'];
    let rword = vec!['S', 'A', 'M', 'X'];
    let mut i = 0;
    let mut ri = 0;

    if vec[pos.0 as usize][pos.1 as usize] == word[i] {
        i += 1;
    }
    if vec[pos.0 as usize][pos.1 as usize] == rword[ri] {
        ri += 1;
    }

    let mut pos = pos;
    while let Some((y, x)) = next(vec, pos, inc) {
        if vec[y][x] == word[i] {
            i += 1;
        }
        if vec[y][x] == rword[ri] {
            ri += 1;
        }
        if ri == 4 {
            ri = 0;
            total += 1;
        }
        if i == 4 {
            i = 0;
            total += 1;
        }
        pos = (y as _, x as _);
    }

    total
}

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
