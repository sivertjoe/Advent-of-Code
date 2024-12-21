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

#[rustfmt::skip]
const KEYPAD_MOVES: [[&str; 5]; 5] = [
/*^ 0*/["", ">", "v<", "v", "v>"], 
/*A 1*/["<", "", "v<<", "<v", "v"], 
/*< 2*/[">^", ">>^", "", ">", ">>"], 
/*v 3*/["^", "^>", "<", "", ">"], 
/*> 4*/["^<", "^", "<<", "<", ""]
];

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

// Too high :( 96396

fn dfs(s: &[u8], mut builder: String, curr: usize, res: &mut Vec<String>) {
    match s {
        [] => res.push(builder),
        [ch, rest @ ..] => {
            let n = (*ch as char).to_digit(16).unwrap() as usize;
            let moves = NUMPAD_MOVES[curr][n];
            let rev = moves.chars().rev().collect::<String>();
            if is_numpad_seq_legal(curr, &rev) {
                let mut builder = builder.clone();
                builder.push_str(&rev);
                builder.push('A');
                dfs(rest, builder, n, res);
            }
            builder.push_str(&moves);
            builder.push('A');
            dfs(rest, builder, n, res);
        }
    }
}

fn dfs2(s: &[u8], mut builder: String, curr: char, res: &mut HashSet<String>, best: Option<usize>) {
    match s {
        [] => {
            res.insert(builder);
        }
        [ch, rest @ ..] => {
            let ch = *ch as char;
            let moves = KEYPAD_MOVES[g(curr)][g(ch)];

            if let Some(best) = best {
                if builder.len() + moves.len() > best {
                    return;
                }
            }

            let rev = moves.chars().rev().collect::<String>();
            if is_keypad_seq_legal(curr, &rev) {
                let mut builder = builder.clone();
                builder.push_str(&rev);
                builder.push('A');
                dfs2(rest, builder, ch, res, best);
            }
            builder.push_str(&moves);
            builder.push('A');
            dfs2(rest, builder, ch, res, best);
        }
    }
}

fn bfs(s: &String) -> usize {
    // Let's do this in steps,
    // The firs robots needs to type, e.g., 029A
    let mut curr = 0xAusize;
    let mut builder = String::new();

    for ch in s.chars() {
        let n = ch.to_digit(16).unwrap() as usize;
        let moves = NUMPAD_MOVES[curr][n];
        builder.push_str(moves);
        builder.push('A');
        curr = n;
    }

    let mut possible = Vec::new();
    dfs(s.as_bytes(), String::new(), 0xA, &mut possible);
    let min = possible.iter().min_by_key(|s| s.len()).unwrap().len();
    let set = possible
        .into_iter()
        .filter(|s| s.len() == min)
        .collect::<HashSet<_>>();

    let mut possible2 = HashSet::new();
    for s in set {
        dfs2(s.as_bytes(), String::new(), 'A', &mut possible2, None);
    }
    let min = possible2.iter().min_by_key(|s| s.len()).unwrap().len();
    let set2 = possible2
        .into_iter()
        .filter(|s| s.len() == min)
        .collect::<HashSet<_>>();

    let mut possible3 = HashSet::new();
    let mut amin = Some(usize::MAX);
    for s in set2 {
        dfs2(s.as_bytes(), String::new(), 'A', &mut possible3, amin);
        let min = possible3.iter().min_by_key(|s| s.len()).unwrap().len();
        amin = amin.map(|amin| amin.min(min));
    }
    let min = possible3.iter().min_by_key(|s| s.len()).unwrap().len();
    min
}

fn part2(s: &String) -> usize {
    0
}

fn number_part(s: &String) -> usize {
    let s: String = s.chars().filter(|ch| ch.is_digit(10)).collect();
    let num = s.parse::<usize>().unwrap();
    num
}

fn task_one(input: &[String]) -> usize {
    input
        .into_iter()
        .map(|line| (line, bfs(line), number_part(line)))
        .inspect(|(line, len, num)| println!("{line}: {len} * {num}"))
        .map(|(line, len, num)| len * num)
        .sum()
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
