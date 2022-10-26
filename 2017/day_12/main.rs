use std::collections::*;

fn parse(input: &[String]) -> Vec<Vec<usize>>
{
    input
        .iter()
        .map(|line| {
            line.split_once("<-> ")
                .unwrap()
                .1
                .split(", ")
                .map(|token| token.parse().unwrap())
                .collect::<Vec<_>>()
        })
        .collect()
}

fn task_one(input: &[String]) -> i32
{
    let input = parse(input);
    let mut count = 1;
    let mut stack = vec![&input[0]];
    let mut seen = HashSet::new();
    seen.insert(0);
    while !stack.is_empty()
    {
        let mut new_stack = Vec::new();
        for reach in stack.into_iter().flatten()
        {
            if seen.insert(*reach)
            {
                count += 1;
                new_stack.push(&input[*reach]);
            }
        }
        stack = new_stack;
    }
    count
}

fn task_two(input: &[String]) -> i32
{
    let input = parse(input);
    let mut count = 0;

    let mut global = HashSet::new();

    for i in 0..input.len()
    {
        let mut stack = vec![&input[i]];
        let mut seen = HashSet::new();
        let mut flag = true;

        seen.insert(i);
        'inner: while !stack.is_empty()
        {
            let mut new_stack = Vec::new();
            for reach in stack.into_iter().flatten()
            {
                if global.contains(reach)
                {
                    // This group is actually part of another group
                    flag = false;
                    break 'inner;
                }
                if seen.insert(*reach)
                {
                    new_stack.push(&input[*reach]);
                }
            }
            stack = new_stack;
        }
        if flag
        {
            count += 1;
        }
        global.extend(seen);
    }
    count
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
