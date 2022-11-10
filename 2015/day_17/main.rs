use std::collections::*;

fn rec<const N: usize>(
    vec: &[usize],
    offset: usize,
    sum: usize,
    taken: usize,
    res: &mut HashMap<usize, usize>,
)
{
    if sum > N
    {
        return;
    }
    else if offset == vec.len()
    {
        if sum == N
        {
            *res.entry(taken).or_default() += 1;
        }
    }
    else
    {
        rec::<N>(vec, offset + 1, sum + vec[offset], taken + 1, res);
        rec::<N>(vec, offset + 1, sum, taken, res)
    }
}

fn task_one(input: &[String]) -> usize
{
    let vec = input.iter().map(|tok| tok.parse::<usize>().unwrap()).collect::<Vec<_>>();

    let mut map = HashMap::new();
    rec::<150>(&vec, 0, 0, 0, &mut map);
    map.into_values().sum()
}

fn task_two(input: &[String]) -> usize
{
    let vec = input.iter().map(|tok| tok.parse::<usize>().unwrap()).collect::<Vec<_>>();

    let mut map = HashMap::new();
    rec::<150>(&vec, 0, 0, 0, &mut map);
    map.into_iter()
        .reduce(|acc, x| std::cmp::min_by_key(acc, x, |x| x.0))
        .unwrap()
        .1
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
