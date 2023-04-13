use std::collections::*;

enum Val
{
    Literal(usize),
    Old,
}
enum Operation
{
    Plus(Val),
    Prod(Val),
}

struct Monkey
{
    items:     VecDeque<usize>,
    operation: Operation,
    test:      [usize; 3],
    inspected: usize,
}

fn parse_oper(s: &str) -> Operation
{
    let mut spl = s.split_whitespace().skip(3);
    let foo = 5;

    let op = spl.next().unwrap();
    let val = spl
        .next()
        .map(|tok| {
            if tok == "old"
            {
                Val::Old
            }
            else
            {
                let lit = tok.parse::<usize>().unwrap();
                Val::Literal(lit)
            }
        })
        .unwrap();

    match op
    {
        "+" => Operation::Plus(val),
        "*" => Operation::Prod(val),
        _ => unreachable!(),
    }
}

fn parse(monkey: &[String]) -> Monkey
{
    let items = monkey[1]
        .split_whitespace()
        .flat_map(|tok| tok.trim_end_matches(',').parse::<usize>())
        .collect();
    let (_, operation) = monkey[2].trim_start().split_once(' ').unwrap();
    let operation = parse_oper(operation);

    let get_num = |n: usize| {
        monkey[n]
            .split_whitespace()
            .flat_map(|tok| tok.parse::<usize>())
            .next()
            .unwrap()
    };

    let test = [3, 4, 5].into_iter().map(get_num).collect::<Vec<_>>().try_into().unwrap();

    Monkey {
        items,
        operation,
        test,
        inspected: 0,
    }
}

#[inline]
fn handle_operation(oper: &Operation, old: usize) -> usize
{
    match oper
    {
        Operation::Prod(Val::Old) => old * old,
        Operation::Prod(Val::Literal(lit)) => old * lit,
        Operation::Plus(Val::Old) => old + old,
        Operation::Plus(Val::Literal(lit)) => old + lit,
    }
}

fn solve<const N: usize, W>(ms: Vec<Monkey>, w: W) -> usize
where
    W: Fn(usize) -> usize,
{
    let mut ms = ms;
    for _ in 0..N
    {
        for i in 0..ms.len()
        {
            let len = ms[i].items.len();
            for _ in 0..len
            {
                let item = ms[i].items.pop_front().unwrap();
                ms[i].inspected += 1;
                let new = handle_operation(&ms[i].operation, item);
                let new = w(new);

                let idx = if new % ms[i].test[0] == 0 { 1 } else { 2 };
                let idx = ms[i].test[idx];

                ms[idx].items.push_back(new);
            }
        }
    }
    let mut ms = ms.into_iter().map(|m| m.inspected).collect::<Vec<_>>();
    ms.sort();
    ms.reverse();
    ms.into_iter().take(2).product()
}

fn task_one(input: &[String]) -> usize
{
    let ms = input.split(|lines| lines.is_empty()).map(parse).collect::<Vec<_>>();
    solve::<20, _>(ms, |n| n / 3)
}

fn task_two(input: &[String]) -> usize
{
    let ms = input.split(|lines| lines.is_empty()).map(parse).collect::<Vec<_>>();
    let div: usize = ms.iter().map(|monkey| monkey.test[0]).product();
    solve::<10000, _>(ms, |n| n % div)
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
