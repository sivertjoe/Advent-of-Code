use std::collections::*;


// Pain peko 🐰
fn comb<T>(vec: Vec<T>, k: usize) -> Vec<Vec<T>>
where
    T: Clone + Copy,
{
    if k == 0
    {
        vec![vec![]]
    }
    else
    {
        vec.iter()
            .enumerate()
            .map(|(i, e)| {
                comb(vec.iter().skip(i + 1).cloned().collect(), k - 1)
                    .into_iter()
                    .map(|c| vec![*e].into_iter().chain(c).collect::<Vec<_>>())
            })
            .flatten()
            .collect()
    }
}

fn solve(nums: &[usize], num_groups: usize) -> usize
{
    let group_size = nums.iter().cloned().sum::<usize>() / num_groups;
    for i in 0..nums.len()
    {
        if let Some(qes) = comb(nums.iter().cloned().collect(), i)
            .into_iter()
            .filter_map(|c| {
                (c.iter().cloned().sum::<usize>() == group_size)
                    .then(|| c.iter().cloned().product::<usize>())
            })
            .min()
        {
            return qes;
        }
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize
{
    let nums = input.iter().map(|num| num.parse::<usize>().unwrap()).collect::<Vec<_>>();
    solve(&nums, 3)
}

fn task_two(input: &[String]) -> usize
{
    let nums = input.iter().map(|num| num.parse::<usize>().unwrap()).collect::<Vec<_>>();
    solve(&nums, 4)
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
