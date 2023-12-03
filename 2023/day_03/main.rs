use std::collections::*;

#[derive(Debug)]
enum Cell {
    Gear(usize),
    Sym(char),
}

#[derive(Debug)]
struct Elem {
    cell: Cell,
    x: usize,
    y: usize,
}

fn parse_schematic(input: &[String]) -> Vec<Elem> {
    let mut elems = Vec::new();
    let mx = input[0].as_bytes().len() - 1;
    for (y, line) in input.iter().enumerate() {
        //let mut vec = Vec::new();
        for (x, ch) in line.bytes().enumerate() {
            if !ch.is_ascii_digit() && ch != b'.' {
                elems.push(Elem {
                    cell: Cell::Sym(ch as char),
                    x,
                    y,
                });
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
                elems.push(Elem {
                    x: index,
                    y,
                    cell: Cell::Gear(g),
                });
                number.clear();
            }
        }
        if !number.is_empty() {
            let g = number.parse().unwrap();
            elems.push(Elem {
                x: index,
                y,
                cell: Cell::Gear(g),
            });
        }
    }
    elems
}

fn task_one(input: &[String]) -> usize {
    let elems = parse_schematic(input);
    let mut sum = 0;
    for elem in elems.iter() {
        if matches!(elem.cell, Cell::Sym(a)) {
            let num = find(elem, &elems);
            sum += num.into_iter().sum::<usize>();
        }
    }

    sum
}

fn find(elem: &Elem, elems: &[Elem]) -> Vec<usize> {
    let mut sum = Vec::new();
    for e in elems {
        if let Cell::Gear(g) = e.cell {
            let len = g.to_string().len();
            let x0 = e.x;
            let x1 = e.x + (len - 1);

            if (x0..=x1)
                .find(|x| e.y.abs_diff(elem.y) <= 1 && x.abs_diff(elem.x) <= 1)
                .is_some()
            {
                sum.push(g)
            }
        }
    }
    sum
}

fn task_two(input: &[String]) -> usize {
    let elems = parse_schematic(input);

    let mut sum = 0;
    for elem in elems.iter() {
        if let Cell::Sym(a) = elem.cell {
            let num = find(elem, &elems);
            if a == '*' && num.len() == 2 {
                sum += num[0] * num[1];
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
