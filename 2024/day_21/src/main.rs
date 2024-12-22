use itertools::Itertools;
use std::collections::*;

#[rustfmt::skip]
const NUMPAD_MOVES: [[&str; 11]; 11] = [
/*0*/["", "^<", "^", ">^", "^^<", "^^", ">^^", "^^^<", "^^^", ">^^^", ">"], 
/*1*/[">v", "", ">", ">>", "^", ">^", ">>^", "^^", ">^^", ">>^^", ">>v"], 
/*2*/["v", "<", "", ">", "<^", "^", ">^", "<^^", "^^", ">^^", ">v"], 
/*3*/["<v", "<<", "<", "", "<<^", "<^", "^", "<<^^", "<^^", "^^", "v"], 
/*4*/[">vv", "v", ">v", ">>v", "", ">", ">>", "^", ">^", ">>^", ">>vv"], 
/*5*/["vv", "<v", "v", ">v", "<", "", ">", "<^", "^", ">^", ">vv"], 
/*6*/["<vv", "<<v", "<v", "v", "<<", "<", "", "<<^", "<^", "^", "vv"], 
/*7*/[">vvv", "vv", ">vv", ">>vv", "v", ">v", ">>v", "", ">", ">>", ">>vvv"], 
/*8*/["vvv", "<vv", "vv", ">vv", "<v", "v", ">v", "<", "", ">", ">vvv"], 
/*9*/["<vvv", "<<vv", "<vv", "vv", "<<v", "<v", "v", "<<", "<", "", "vvv"], 
/*A*/["<", "^<<", "<^", "^", "^^<<", "<^^", "^^", "^^^<<", "<^^^", "^^^", ""]
];

#[rustfmt::skip]
const KEYPAD_MOVES: [[&str; 5]; 5] = [
/*^ 0*/["", ">", "v<", "v", "v>"], 
/*A 1*/["<", "", "v<<", "<v", "v"], 
/*< 2*/[">^", ">>^", "", ">", ">>"], 
/*v 3*/["^", "^>", "<", "", ">"], 
/*> 4*/["<^", "^", "<<", "<", ""]
];

fn is_numpad_seq_legal(start: usize, path: &str) -> bool {
    match start {
        0 => !path.starts_with("<"),
        0xA => !path.starts_with("<<"),
        1 => !path.starts_with("v"),
        4 => !path.starts_with("vv"),
        7 => !path.starts_with("vvv"),
        _ => true,
    }
}

fn is_keypad_seq_legal(start: char, path: &str) -> bool {
    match start {
        '<' => !path.starts_with("^"),
        '^' => !path.starts_with("<"),
        'A' => !path.starts_with("<<"),
        _ => true,
    }
}

fn g(ch: char) -> usize {
    match ch {
        '^' => 0,
        'A' => 1,
        '<' => 2,
        'v' => 3,
        '>' => 4,
        _ => unreachable!(),
    }
}

fn perms<F>(s: &str, filter: F) -> impl Iterator<Item = String>
where
    F: Fn(&String) -> bool + Copy,
{
    let vec = s.chars().collect::<Vec<_>>();
    let len = vec.len();
    vec.into_iter()
        .permutations(len)
        .unique()
        .map(|v| v.into_iter().collect::<String>())
        .filter(filter)
}

fn _dfs(s: &str, depth: usize, cache: &mut HashMap<(String, usize), usize>) -> usize {
    if depth == 0 {
        return s.len();
    }

    if let Some(cached) = cache.get(&(s.to_string(), depth)) {
        return *cached;
    }

    let mut sum = 0;
    for (from, to) in std::iter::once('A').chain(s.chars()).tuple_windows() {
        let moves = KEYPAD_MOVES[g(from)][g(to)];

        sum += perms(moves, |s| is_keypad_seq_legal(from, s))
            .map(|s| _dfs(&format!("{s}A"), depth - 1, cache))
            .min()
            .unwrap();
    }
    cache.insert((s.to_string(), depth), sum);
    sum
}

fn dfs(input: &str, depth: usize, cache: &mut HashMap<(String, usize), usize>) -> usize {
    std::iter::once('A')
        .chain(input.chars())
        .map(|ch| ch.to_digit(16).unwrap() as usize)
        .tuple_windows()
        .map(|(a, b)| {
            let moves = NUMPAD_MOVES[a][b];
            perms(moves, |s| is_numpad_seq_legal(a, s))
                .map(|s| _dfs(&format!("{s}A"), depth, cache))
                .min()
                .unwrap()
        })
        .sum()
}

fn solve<const N: usize>(input: &[String]) -> usize {
    let mut cache = HashMap::<(String, usize), usize>::default();
    input
        .iter()
        .map(|line| number_part(line) * dfs(line, N, &mut cache))
        .sum()
}

fn number_part(s: &str) -> usize {
    let s: String = s.chars().filter(|ch| ch.is_ascii_digit()).collect();
    s.parse::<usize>().unwrap()
}

fn task_one(input: &[String]) -> usize {
    solve::<2>(input)
}

fn task_two(input: &[String]) -> usize {
    solve::<25>(input)
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
