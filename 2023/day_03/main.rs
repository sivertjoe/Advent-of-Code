use std::collections::*;

enum Cell {
    Gear(usize),
    Sym(char),
}

struct Elem {
    cell: Cell,
    x: usize,
    y: usize,
}

struct Number(usize);
struct Symbol(char);

fn parse_schematic(input: &[String]) -> (Vec<Vec<(Number, usize)>>, Vec<(Symbol, usize, usize)>) {
    let mut nums = Vec::new();
    let mut syms = Vec::new();
    let mx = input[0].as_bytes().len() - 1;
    for (y, line) in input.iter().enumerate() {
        let mut lnums = Vec::new();

        for (x, ch) in line.bytes().enumerate() {
            if !ch.is_ascii_digit() && ch != b'.' {
                syms.push((Symbol(ch as char), x, y));
            }
        }

        let mut number = String::new();
        let mut index = 0;

        for (x, ch) in line.bytes().enumerate() {
            if ch.is_ascii_digit() {
                if number.is_empty() {
                    index = x;
                }
                number.push(ch as char);
            } else if !number.is_empty() {
                let g = number.parse().unwrap();
                lnums.push((Number(g), index));
                number.clear();
            }
        }
        if !number.is_empty() {
            let g = number.parse().unwrap();
            lnums.push((Number(g), index));
        }

        nums.push(lnums);
    }
    (nums, syms)
}

fn find((x, y): (usize, usize), numbers: &[Vec<(Number, usize)>]) -> Vec<usize> {
    let mut vec = Vec::with_capacity(4);
    for y in [
        y.saturating_sub(1),
        y,
        std::cmp::min(numbers.len() - 1, y + 1),
    ] {
        for (num, _x) in &numbers[y] {
            let len = num.0.to_string().len();
            let x0 = *_x;
            let x1 = *_x + (len - 1);

            if (x0..=x1).find(|nx| nx.abs_diff(x) <= 1).is_some() {
                vec.push(num.0);
            }
        }
    }
    vec
}

fn task_one(input: &[String]) -> usize {
    let (numbers, symbols) = parse_schematic(input);

    symbols
        .into_iter()
        .flat_map(|(_sym, x, y)| find((x, y), &numbers).into_iter())
        .sum()
}

fn task_two(input: &[String]) -> usize {
    let (numbers, symbols) = parse_schematic(input);

    symbols
        .into_iter()
        .filter_map(|(sym, x, y)| {
            if sym.0 == '*' {
                let nums = find((x, y), &numbers);
                if nums.len() == 2 {
                    Some(nums[0] * nums[1])
                } else {
                    None
                }
            } else {
                None
            }
        })
        .sum()

    /*let mut sum = 0;
    for (sym, x, y) in symbols {
        if sym.0 == '*' {
            let nums = find((x, y), &numbers);
            if nums.len() == 2 {
                sum += nums[0] * nums[1];
            }
        }
    }
    sum*/
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
