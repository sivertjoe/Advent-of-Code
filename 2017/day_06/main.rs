use std::collections::*;
use std::cmp::Ordering;

fn solve(input: &[String]) -> (usize, usize)
{    
    let mut nums: Vec<usize> = input[0].split_whitespace().map(|token|token.parse().unwrap()).collect();
    let mut seen = HashMap::new();
    seen.insert(nums.clone(), 0);

    for cycles in 1..
    {
        let idx = nums
                   .iter()
                   .enumerate()
                   .max_by(|(_, a), (_, b)| (a >= b).then_some(Ordering::Greater).unwrap_or(Ordering::Less))
                   .unwrap().0;

        let high = nums[idx];
        nums[idx] = 0;
        let len = nums.len();
        for i in 1..=high
        {
            nums[(idx + i) % len] += 1;
        }

        if let Some(idx) = seen.get(&nums)
        {
            return (cycles, cycles - idx);
        }
        else
        {
            seen.insert(nums.clone(), cycles);
        }
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize
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
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
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
