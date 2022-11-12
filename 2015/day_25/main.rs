use std::collections::*;

fn parse(input: &[String]) -> (usize, usize)
{
    let mut iter = input[0].split_whitespace();

    let mut get = |n: usize| {
        let s = iter.nth(n).unwrap();
        s[..s.len() - 1].parse().unwrap()
    };
    (get(15), get(1))
}

fn task_one(input: &[String]) -> usize
{
    let (n1, n2) = parse(input);
    let mut x = 1;
    let mut y = 1;
    let mut code = 20151125;

    loop
    {
        if y == 1
        {
            y = x + 1;
            x = 1;
        }
        else
        {
            y -= 1;
            x += 1;
        }

        code = (code * 252533) % 33554393;
        if x == n2 && y == n1
        {
            return code;
        }
    }
}

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
