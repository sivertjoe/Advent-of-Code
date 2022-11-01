use std::collections::*;

fn parse(input: &[String]) -> Vec<(Dir, i32)>
{
    input[0]
        .split(", ")
        .map(|tok| {
            (
                if tok[0..=0].starts_with('R') { Dir::Right } else { Dir::Left },
                tok[1..].parse::<i32>().unwrap(),
            )
        })
        .collect()
}

#[allow(dead_code)]
#[repr(i32)]
#[derive(Clone, Copy)]
enum Dir
{
    Up = 0,
    Right,
    Down,
    Left,
}

fn forward(p: (i32, i32), dir: &Dir) -> (i32, i32)
{
    match dir
    {
        Dir::Left => (p.0 - 1, p.1),
        Dir::Right => (p.0 + 1, p.1),
        Dir::Down => (p.0, p.1 + 1),
        Dir::Up => (p.0, p.1 - 1),
    }
}

fn turn(dir: &Dir, turn_dir: Dir) -> Dir
{
    let dir = *dir as i32;
    let turn_dir = if let Dir::Left = turn_dir { -1 } else { 1 };
    let dir = (dir + turn_dir).rem_euclid(4);
    // SAFETY:
    // 🙏
    unsafe { std::mem::transmute(dir) }
}

fn task_one(input: &[String]) -> i32
{
    let mut facing = Dir::Up;
    let mut pos = (0, 0);

    for (dir, steps) in parse(input)
    {
        facing = turn(&facing, dir);
        for _ in 0..steps
        {
            pos = forward(pos, &facing);
        }
    }
    pos.0.abs() + pos.1.abs()
}

fn task_two(input: &[String]) -> i32
{
    let mut facing = Dir::Up;
    let mut pos = (0, 0);

    let mut seen = HashSet::new();

    for (dir, steps) in parse(input)
    {
        facing = turn(&facing, dir);
        for _ in 0..steps
        {
            pos = forward(pos, &facing);
            if !seen.insert(pos)
            {
                return pos.0.abs() + pos.1.abs();
            }
        }
    }
    pos.0.abs() + pos.1.abs()
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
