#[derive(PartialEq, Clone, Copy)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}
use Hand::*;

fn point(h: Hand) -> usize {
    match h {
        Rock => 1,
        Paper => 2,
        Scissors => 3,
    }
}

fn get(h: &str) -> Hand {
    match h {
        "A" | "X" => Rock,
        "B" | "Y" => Paper,
        "C" | "Z" => Scissors,
        _ => unreachable!(),
    }
}

fn task_one(input: &[String]) -> usize {
    let round_point = |other, me| match (me, other) {
        (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6,
        (x, y) if x == y => 3,
        _ => 0,
    };

    input
        .iter()
        .map(|line| line.split_once(' ').map(|(a, b)| (get(a), get(b))).unwrap())
        .fold(0, |acc, (other, me)| {
            acc + point(me) + round_point(other, me)
        })
}

fn task_two(input: &[String]) -> usize {
    const LOOSE: &str = "X";
    const DRAW: &str = "Y";
    const WIN: &str = "Z";

    input
        .iter()
        .map(|line| line.split_once(' ').map(|(a, b)| (get(a), b)).unwrap())
        .fold(0, |acc, (a, b)| {
            acc + match (b, a) {
                (LOOSE, x) => (point(x) as i32 - 2).rem_euclid(3) as usize + 1,
                (DRAW, x) => 3 + point(x),
                (WIN, x) => 6 + (point(x) % 3) + 1,
                _ => unreachable!(),
            }
        })
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
    let elapsed = t.elapsed().as_millis();

    match task {
        Task::One => {
            println!("({}ms)\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}ms)\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
