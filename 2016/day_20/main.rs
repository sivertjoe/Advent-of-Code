fn task_one(input: &[String]) -> u32
{
    let mut vec = input
        .iter()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            let a = a.parse::<u32>().unwrap();
            let b = b.parse::<u32>().unwrap();
            (a, b)
        })
        .collect::<Vec<_>>();
    vec.sort();

    for i in 1..vec.len()
    {
        if vec[i - 1].1 + 1 < vec[i].0
        {
            return vec[i - 1].1 + 1;
        }
    }
    0
}

fn task_two(input: &[String]) -> u64
{
    let mut vec = input
        .iter()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            let a = a.parse::<u64>().unwrap();
            let b = b.parse::<u64>().unwrap();
            (a, b)
        })
        .collect::<Vec<_>>();
    vec.sort();

    let mut count = 0;
    let (mut mn, mut mx) = vec[0];

    for int in vec
    {
        if int.0 > mx + 1
        {
            count += mx - mn + 1;
            mn = int.0;
            mx = int.1;
        }
        else
        {
            mx = std::cmp::max(mx, int.1);
        }
    }

    u32::MAX as u64 - count - (mx - mn + 1) + 1
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
