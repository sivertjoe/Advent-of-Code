use std::collections::*;

fn esc_len(s: &str) -> usize
{
    let mut s = &s[1..s.len() - 1];
    let mut count = 0;

    while !s.is_empty()
    {
        if s.starts_with('\\')
        {
            if s[1..].starts_with('x')
            {
                s = &s[4..];
            }
            else
            {
                s = &s[2..];
            }
        }
        else
        {
            s = &s[1..];
        }
        count += 1;
    }
    count
}

fn task_one(input: &[String]) -> usize
{
    input.iter().map(|line| line.len() - esc_len(line)).sum()
}

fn task_two(input: &[String]) -> usize
{
    input.iter().map(|line| format!("{:?}", line).len() - line.len()).sum()
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
