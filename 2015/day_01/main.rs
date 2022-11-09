fn solve(input: &[String], early_exit: bool) -> isize
{
    let mut count = 0;
    for (i, ch) in input[0].chars().enumerate()
    {
        if ch == '('
        {
            count += 1;
        }
        else
        {
            count -= 1;
        }

        if count == -1 && early_exit
        {
            return (i + 1) as isize;
        }
    }
    count
}
fn task_one(input: &[String]) -> isize
{
    solve(input, false)
}

fn task_two(input: &[String]) -> isize
{
    solve(input, true)
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
