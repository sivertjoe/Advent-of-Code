fn solve(input: &[String], f: impl Fn(&mut i32)) -> usize
{
    let mut input: Vec<i32> = input.iter().map(|token| token.parse().unwrap()).collect();
    let mut idx = 0;
    let mut steps = 0;

    while let Some(num) = input.get_mut(idx as usize)
    {
        idx += *num;
        f(num);
        steps += 1
    }
    steps
}

fn task_one(input: &[String]) -> usize
{
    solve(input, |v: &mut i32| *v += 1)
}
fn task_two(input: &[String]) -> usize
{
    let arr = [1, -1];
    solve(input, |v: &mut i32| *v += arr[(*v >= 3) as usize])
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
