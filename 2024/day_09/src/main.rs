use std::collections::*;

fn generate(input: &[String]) -> Vec<isize> {
    let mut iter = input[0].chars();
    let mut vec = Vec::new();

    let mut i = 0;
    loop {
        let Some(n) = iter.next() else { break };
        let n = n.to_digit(10).unwrap();
        vec.append(&mut (0..n).map(|_| i).collect());
        i += 1;
        let Some(n) = iter.next() else { break };
        let n = n.to_digit(10).unwrap();
        vec.append(&mut (0..n).map(|_| -1).collect());
    }

    vec
}

fn checksum(vec: &[isize]) -> usize {
    vec.into_iter()
        .take_while(|n| **n != -1)
        .enumerate()
        .map(|(i, n)| i * *n as usize)
        .sum()
}

fn task_one(input: &[String]) -> usize {
    let mut vec = generate(input);

    let mut i = 0;
    let mut j = vec.len() - 1;
    loop {
        while i < vec.len() {
            if vec[i] == -1 {
                break;
            }
            i += 1;
        }
        while j >= 0 {
            if vec[j] != -1 {
                break;
            }
            j -= 1;
        }

        if i >= j {
            break;
        }

        let temp = vec[i];
        vec[i] = vec[j];
        vec[j] = temp;
    }
    checksum(&vec)
}

fn find_next_empty_section(curr: usize, vec: &[isize]) -> (usize, usize) {
    let i = curr + 1;
    while i < vec.len() {
        if vec[i] == -1 {}
    }

    let ii = i;
    todo!()
}

fn task_two(input: &[String]) -> usize {
    let mut vec = generate(input);

    0
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
