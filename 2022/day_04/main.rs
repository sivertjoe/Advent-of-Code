use std::collections::*;

#[inline]
fn parse(line: &str) -> (HashSet<usize>, HashSet<usize>)
{
    let parse = |s: &str| {
        let (v1, v2) = s.split_once('-').unwrap();
        let v1 = v1.parse::<usize>().unwrap();
        let v2 = v2.parse::<usize>().unwrap();

        (v1..=v2).collect::<HashSet<_>>()
    };

    let (s1, s2) = line.split_once(',').unwrap();
    (parse(s1), parse(s2))
}

fn solve<F>(input: &[String], f: F) -> usize
where
    F: Fn((HashSet<usize>, HashSet<usize>)) -> bool,
{
    input.iter().filter(|line| f(parse(line))).count()
}

fn task_one(input: &[String]) -> usize
{
    solve(input, |(s1, s2)| s1.is_subset(&s2) || s2.is_subset(&s1))
}

fn task_two(input: &[String]) -> usize
{
    solve(input, |(s1, s2)| !s1.is_disjoint(&s2))
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
