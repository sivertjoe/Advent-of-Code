use std::collections::*;

fn walk<I>(seen: &mut HashSet<(isize, isize)>, iter: I)
where
    I: Iterator<Item = char>,
{
    let mut x = 0;
    let mut y = 0;
    for ch in iter
    {
        match ch
        {
            '^' => y -= 1,
            'v' => y += 1,
            '>' => x += 1,
            '<' => x -= 1,
            _ => unreachable!(),
        }

        seen.insert((x, y));
    }
}

fn task_one(input: &[String]) -> usize
{
    let mut seen: HashSet<(isize, isize)> = HashSet::new();
    seen.insert((0, 0));
    walk(&mut seen, input[0].chars());
    seen.len()
}

fn task_two(input: &[String]) -> usize
{
    let mut seen: HashSet<(isize, isize)> = HashSet::new();
    seen.insert((0, 0));
    walk(&mut seen, input[0].chars().step_by(2));
    walk(&mut seen, input[0].chars().skip(1).step_by(2));
    seen.len()
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
