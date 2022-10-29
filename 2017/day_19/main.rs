use std::collections::*;

fn parse(input: &[String]) -> HashMap<(usize, usize), char>
{
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate()
    {
        for (x, ch) in line.chars().enumerate()
        {
            if !ch.is_whitespace()
            {
                map.insert((x, y), ch);
            }
        }
    }
    map
}

#[derive(Debug)]
enum Dir
{
    Up,
    Down,
    Left,
    Right,
}

fn opposite(dir: &Dir, point: (usize, usize)) -> (usize, usize)
{
    match dir
    {
        Dir::Up => (point.0, point.1 + 1),
        Dir::Down => (point.0, point.1 - 1),
        Dir::Right => (point.0 - 1, point.1),
        Dir::Left => (point.0 + 1, point.1),
    }
}

fn next_dir(dir: &Dir, point: (usize, usize), map: &HashMap<(usize, usize), char>) -> Dir
{
    let from = opposite(dir, point);
    let ch = map[&from];

    let p = *[
        (point.0, point.1 - 1),
        (point.0, point.1 + 1),
        (point.0 - 1, point.1),
        (point.0 + 1, point.1),
    ]
    .iter()
    .find(|p| match map.get(p)
    {
        Some(char) => *char != ch,
        _ => false,
    })
    .unwrap();
    if p.0 < point.0
    {
        Dir::Left
    }
    else if p.0 > point.0
    {
        Dir::Right
    }
    else if p.1 < point.1
    {
        Dir::Up
    }
    else
    {
        Dir::Down
    }
}

fn next_point(p: (usize, usize), dir: &Dir) -> (usize, usize)
{
    match dir
    {
        Dir::Right => (p.0 + 1, p.1),
        Dir::Left => (p.0 - 1, p.1),
        Dir::Up => (p.0, p.1 - 1),
        Dir::Down => (p.0, p.1 + 1),
    }
}

fn solve(input: &[String]) -> (String, usize)
{
    let map = parse(input);
    let mut pos = *map
        .iter()
        .find_map(|(key, val)| (*val == '|' && key.1 == 0).then_some(key))
        .unwrap();

    let mut dir = Dir::Down;

    let mut letters = String::new();
    let mut steps = 0;
    loop
    {
        steps += 1;
        pos = next_point(pos, &dir);
        match map.get(&pos)
        {
            Some(ch) => match ch
            {
                c if c.is_ascii_alphabetic() => letters.push(*c),
                '+' =>
                {
                    dir = next_dir(&dir, pos, &map);
                },
                _ =>
                {},
            },
            _ => break,
        }
    }
    (letters, steps)
}

fn task_one(input: &[String]) -> String
{
    solve(input).0
}

fn task_two(input: &[String]) -> usize
{
    solve(input).1
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path).unwrap().lines().map(String::from).collect()
}

enum Task
{
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

    match task
    {
        Task::One =>
        {
            println!("({}ms)\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        },
        Task::Two =>
        {
            println!("({}ms)\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        },
    };
}

fn get_input_file() -> String
{
    std::env::args().nth(1).unwrap_or_else(|| "input".to_string())
}
