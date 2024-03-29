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

fn solve(input: &[String], f: impl Fn(&[i32]) -> i32) -> i32
{
    input
        .iter()
        .map(|line| {
            let nums: Vec<i32> =
                line.split_whitespace().map(|token| token.parse().unwrap()).collect();
            f(&nums)
        })
        .sum()
}

fn task_one(input: &[String]) -> i32
{
    solve(input, |nums| nums.iter().max().unwrap() - nums.iter().min().unwrap())
}

fn find_divisible(nums: &[i32]) -> i32
{
    for num1 in nums
    {
        for num2 in nums
        {
            if num1 == num2
            {
                continue;
            }
            if num1 % num2 == 0
            {
                return num1 / num2;
            }
        }
    }
    unreachable!()
}

fn task_two(input: &[String]) -> i32
{
    solve(input, find_divisible)
}
