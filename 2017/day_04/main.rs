use std::collections::*;

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

fn main()
{
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn task_one(input: &[String]) -> usize
{
    input
        .iter()
        .filter(|line| {
            let split: Vec<&str> = line.split_whitespace().collect();
            let len = split.len();
            split.into_iter().collect::<HashSet<&str>>().len() == len
        })
        .count()
}

fn task_two(input: &[String]) -> usize
{
    input
        .iter()
        .filter(|line| {
            let split: Vec<&str> = line.split_whitespace().collect();
            let len = split.len();
            let mut set = HashSet::new();
            for word in &split
            {
                let mut arr = [0u8; 26];
                for ch in word.bytes()
                {
                    let idx = (ch - b'a') as usize;
                    arr[idx] += 1;
                }
                set.insert(arr);
            }
            set.len() == len
        })
        .count()
}
