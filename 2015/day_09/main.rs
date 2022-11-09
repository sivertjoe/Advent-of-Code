use std::collections::*;

fn unique_permutations<T: Clone>(items: Vec<T>) -> Vec<Vec<T>>
where
    T: Ord,
{
    if items.len() == 1
    {
        vec![items]
    }
    else
    {
        let mut output: Vec<Vec<T>> = vec![];

        let mut unique_items = items.clone();
        unique_items.sort();
        unique_items.dedup();
        for first in unique_items
        {
            let mut remaining_elements = items.clone();

            let index = remaining_elements.iter().position(|x| *x == first).unwrap();
            remaining_elements.remove(index);

            for mut permutation in unique_permutations(remaining_elements)
            {
                permutation.insert(0, first.clone());
                output.push(permutation);
            }
        }
        output
    }
}

fn solve<Cmp>(input: &[String], cmp: Cmp, init: usize) -> usize
where
    Cmp: Fn(usize, usize) -> usize,
{
    let mut dist: HashMap<String, HashMap<String, usize>> = HashMap::new();
    for line in input
    {
        let mut iter = line.split_whitespace();
        let src = iter.next().unwrap().to_string();
        let dst = iter.nth(1).unwrap().to_string();
        let cost = iter.nth(1).unwrap().parse::<usize>().unwrap();

        dist.entry(src.clone()).or_default().insert(dst.clone(), cost);
        dist.entry(dst).or_default().insert(src, cost);
    }

    let cities = dist.keys().cloned().collect::<Vec<_>>();
    let mut distance = init;
    for route in unique_permutations(cities)
    {
        let mut cost = 0;
        let mut pos = route[0].clone();

        for city in route.into_iter().skip(1)
        {
            cost += dist[&pos][&city];
            pos = city;
        }
        distance = cmp(distance, cost);
    }
    distance
}


fn task_one(input: &[String]) -> usize
{
    solve(input, std::cmp::min, usize::MAX)
}

fn task_two(input: &[String]) -> usize
{
    solve(input, std::cmp::max, 0)
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
