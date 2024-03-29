use std::collections::*;


fn task_one(input: &[String]) -> u32
{
    // This is the Josephus Problem
    // https://www.youtube.com/watch?v=uCsD3ZGzMgE
    let n = input[0].parse::<u32>().unwrap();
    let l = n - n.next_power_of_two() / 2;

    2 * l + 1
}

fn task_two(input: &[String]) -> u32
{
    let n = input[0].parse::<u32>().unwrap();

    let mut left: VecDeque<_> = (0..n / 2 + 1).collect();
    let mut right: VecDeque<_> = (n / 2 + 1..=n).collect();

    while !left.is_empty() && !right.is_empty()
    {
        if left.len() > right.len()
        {
            left.pop_back();
        }
        else
        {
            right.pop_back();
        }
        right.push_front(left.pop_front().unwrap());
        left.push_back(right.pop_back().unwrap());
    }
    left.pop_front().unwrap()
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
