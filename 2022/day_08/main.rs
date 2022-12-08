use std::collections::*;

fn create_map(input: &[String]) -> HashMap<(usize, usize), u32>
{
    input
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().map(move |(x, ch)| ((x, y), ch.to_digit(10).unwrap()))
        })
        .collect()
}

fn count(
    tree: u32,
    map: &HashMap<(usize, usize), u32>,
    iter: impl Iterator<Item = (usize, usize)>,
) -> usize
{
    let mut c = 0;
    for (x, y) in iter
    {
        c += 1;
        if *map.get(&(x, y)).unwrap() >= tree
        {
            return c;
        }
    }
    c
}

fn task_one(input: &[String]) -> usize
{
    let map = create_map(input);

    let len_x = input[0].len();
    let len_y = input.len();

    map.iter()
        .filter(|(k, tree)| {
            (0..k.0).all(|x| map.get(&(x, k.1)).unwrap() < tree)
                || (k.0 + 1..len_x).all(|x| map.get(&(x, k.1)).unwrap() < tree)
                || (0..k.1).all(|y| map.get(&(k.0, y)).unwrap() < tree)
                || (k.1 + 1..len_y).all(|y| map.get(&(k.0, y)).unwrap() < tree)
        })
        .count()
}


fn task_two(input: &[String]) -> usize
{
    let map = create_map(input);

    let len_x = input[0].len();
    let len_y = input.len();

    map.iter()
        .map(|(k, tree)| {
            [
                count(*tree, &map, (0..k.0).rev().map(|x| (x, k.1))),
                count(*tree, &map, (k.0 + 1..len_x).map(|x| (x, k.1))),
                count(*tree, &map, (0..k.1).rev().map(|y| (k.0, y))),
                count(*tree, &map, (k.1 + 1..len_y).map(|y| (k.0, y))),
            ]
            .into_iter()
            .product::<usize>()
        })
        .max()
        .unwrap()
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
