use std::collections::*;

fn parse_game(line: &str) -> (isize, Vec<Vec<(String, isize)>>) {
    let line = line.replace(',', "");
    let mut split = line.split(": ");
    let id = split
        .next()
        .unwrap()
        .split(' ')
        .nth(1)
        .unwrap()
        .parse::<isize>()
        .unwrap();

    let mut semi = split.next().unwrap().split("; ");
    let fin = semi
        .map(|sub| {
            let mut v = Vec::new();
            let mut iter = sub.split(' ');
            while let (Some(num), Some(color)) = (iter.next(), iter.next()) {
                let num = num.parse::<isize>().unwrap();
                let color = color.to_string();
                v.push((color, num));
            }
            v
        })
        .collect::<Vec<_>>();

    (id, fin)
}

fn task_one(input: &[String]) -> isize {
    let mut iter = input.iter().map(|n| parse_game(n));
    let mut counter = 0;
    'gam: for game in iter {
        for set in game.1 {
            let mut red = 12;
            let mut green = 13;
            let mut blue = 14;

            for (color, num) in set {
                match color.as_str().trim() {
                    "blue" => blue -= num,
                    "red" => red -= num,
                    "green" => green -= num,
                    e => panic!("??{e}??"),
                }
            }
            if red < 0 || green < 0 || blue < 0 {
                continue 'gam;
            }
        }
        counter += game.0;
    }
    counter
}

fn task_two(input: &[String]) -> isize {
    let mut iter = input.iter().map(|n| parse_game(n));
    let mut counter = 0;
    for game in iter {
        let mut mred = 0;
        let mut mgreen = 0;
        let mut mblue = 0;
        for set in game.1 {
            let mut red = 0;
            let mut green = 0;
            let mut blue = 0;

            for (color, num) in set {
                match color.as_str().trim() {
                    "blue" => blue += num,
                    "red" => red += num,
                    "green" => green += num,
                    e => panic!("??{e}??"),
                }
            }
            mred = mred.max(red);
            mgreen = mgreen.max(green);
            mblue = mblue.max(blue);
        }
        counter += mred * mgreen * mblue;
    }
    counter
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
