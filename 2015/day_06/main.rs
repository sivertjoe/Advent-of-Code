use std::collections::*;

#[derive(Clone, Copy)]
enum Type
{
    Toggle,
    Switch(bool),
}

struct Command
{
    start: (usize, usize),
    end:   (usize, usize),
    typ:   Type,
}


fn parse_line(line: &String) -> Command
{
    let mut iter = line.split_whitespace();
    let mut f = |x, y| {
        let fst = iter.nth(x).unwrap();
        let start = fst
            .split_once(',')
            .map(|(a, b)| (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap()))
            .unwrap();
        let snd = iter.nth(y).unwrap();
        let end = snd
            .split_once(',')
            .map(|(a, b)| (a.parse::<usize>().unwrap(), b.parse::<usize>().unwrap()))
            .unwrap();
        (start, end)
    };

    if line.starts_with("toggle")
    {
        let (start, end) = f(1, 1);
        Command {
            start,
            end,
            typ: Type::Toggle,
        }
    }
    else
    {
        let on = line.split_whitespace().nth(1).unwrap() == "on";
        let (start, end) = f(2, 1);

        Command {
            start,
            end,
            typ: Type::Switch(on),
        }
    }
}

fn solve<F, T>(input: &[String], f: F) -> HashMap<(usize, usize), T>
where
    F: Fn(&mut HashMap<(usize, usize), T>, usize, usize, Type),
    T: Default,
{
    let mut map: HashMap<(usize, usize), T> = HashMap::new();
    for y in 0..1000
    {
        for x in 0..1000
        {
            map.insert((x, y), T::default());
        }
    }


    let cmds = input.iter().map(parse_line).collect::<Vec<_>>();
    for cmd in cmds
    {
        for y in cmd.start.1..=cmd.end.1
        {
            for x in cmd.start.0..=cmd.end.0
            {
                f(&mut map, x, y, cmd.typ);
            }
        }
    }

    map
}

fn task_one(input: &[String]) -> usize
{
    let f = |map: &mut HashMap<(usize, usize), bool>, x: usize, y: usize, typ: Type| match typ
    {
        Type::Toggle =>
        {
            map.insert((x, y), !map[&(x, y)]);
        },
        Type::Switch(b) =>
        {
            map.insert((x, y), b);
        },
    };
    let map = solve(input, f);

    map.into_values().filter(|b| *b).count()
}

fn task_two(input: &[String]) -> usize
{
    let f = |map: &mut HashMap<(usize, usize), usize>, x: usize, y: usize, typ: Type| match typ
    {
        Type::Toggle =>
        {
            map.insert((x, y), map[&(x, y)] + 2);
        },
        Type::Switch(b) =>
        {
            if b
            {
                map.insert((x, y), map[&(x, y)] + 1);
            }
            else
            {
                map.insert((x, y), map[&(x, y)].saturating_sub(1));
            }
        },
    };
    let map = solve(input, f);
    map.into_values().sum()
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
