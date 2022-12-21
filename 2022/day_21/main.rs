use std::collections::*;

#[derive(Clone, Copy)]
enum Op
{
    Plus,
    Minus,
    Div,
    Mul,
}

enum Noise
{
    Num(isize),
    Operation(Op, String, String),
}

fn parse_oper(line: &str) -> Noise
{
    let mut iter = line.split_whitespace();
    let left = iter.next();
    let left = left.unwrap().to_string();
    if let Some(op) = iter.next()
    {
        let right = iter.next().unwrap();
        let right = right.to_string();
        let op = match op
        {
            "+" => Op::Plus,
            "-" => Op::Minus,
            "/" => Op::Div,
            "*" => Op::Mul,
            _ => unreachable!(),
        };

        Noise::Operation(op, left, right)
    }
    else
    {
        let num = left.parse().unwrap();
        Noise::Num(num)
    }
}

fn parse(input: &[String]) -> HashMap<String, Noise>
{
    input
        .iter()
        .map(|line| {
            let (name, body) = line.split_once(": ").unwrap();
            let name = name.to_string();
            (name, parse_oper(body))
        })
        .collect()
}

fn exec(op: Op, left: isize, right: isize) -> isize
{
    match op
    {
        Op::Plus => left + right,
        Op::Minus => left - right,
        Op::Mul => left * right,
        Op::Div => left / right,
    }
}

fn rec(map: &HashMap<String, Noise>, root: String, cache: &mut HashMap<String, isize>) -> isize
{
    match map.get(&root).unwrap()
    {
        Noise::Num(n) => *n,
        Noise::Operation(op, left, right) =>
        {
            let mut get = |s: &str| {
                if let Some(val) = cache.get(s)
                {
                    *val
                }
                else
                {
                    let num = rec(map, s.to_string(), cache);
                    cache.insert(s.to_string(), num);
                    num
                }
            };
            let left = get(left);
            let right = get(right);
            let num = exec(*op, left, right);
            cache.insert(root.to_string(), num);
            num
        },
    }
}

#[derive(Debug)]
enum Dir
{
    Left,
    Right,
}

fn build_operations(
    map: &HashMap<String, Noise>,
    cache: &HashMap<String, isize>,
) -> Vec<(Op, isize, Dir)>
{
    let mut opers = Vec::new();
    let mut s = "humn".to_string();
    while s != "root"
    {
        let elem = map
            .iter()
            .find_map(|(k, v)| match v
            {
                Noise::Operation(op, left, right) if *left == s || *right == s =>
                {
                    // E.g:
                    // <expr> = {s} <op> <variable>
                    // Or
                    // <expr> = <variable> <op> {s}

                    let other = if *left == s { right } else { left };
                    let val = cache.get(other).unwrap();
                    let dir = if *left == s { Dir::Left } else { Dir::Right };
                    Some((k.to_string(), *op, *val, dir))
                },
                _ => None,
            })
            .unwrap();

        s = elem.0.clone();
        opers.push((elem.1, elem.2, elem.3));
    }
    opers
}

fn task_one(input: &[String]) -> isize
{
    let map = parse(input);
    rec(&map, "root".to_string(), &mut HashMap::new())
}

fn task_two(input: &[String]) -> isize
{
    let map = parse(input);
    let mut cache = HashMap::new();
    rec(&map, "root".to_string(), &mut cache);

    let opers = build_operations(&map, &cache);

    let acc = opers.last().unwrap().1;
    opers.into_iter().rev().skip(1).fold(acc, |acc, (op, val, dir)| match (op, &dir)
    {
        (Op::Plus, _) => acc - val,
        (Op::Mul, _) => acc / val,
        (Op::Div, Dir::Right) => val / acc,
        (Op::Div, Dir::Left) => acc * val,
        (Op::Minus, Dir::Right) => val - acc,
        (Op::Minus, Dir::Left) => acc + val,
    })
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
