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

fn solve(input: &[String], inc: usize) -> u32
{
    let nums: Vec<u32> = input[0].chars().flat_map(|c| c.to_digit(10)).collect();
    let len = nums.len();
    nums.iter()
        .enumerate()
        .filter_map(|(i, current)| {
            let next = nums[(i + inc) % len];
            (*current == next).then_some(current)
        })
        .sum()
}

fn task_one(input: &[String]) -> u32
{
    solve(input, 1)
}

fn task_two(input: &[String]) -> u32
{
    let inc = input[0].len() / 2;
    solve(input, inc)
}
