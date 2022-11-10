use std::collections::*;

fn create_map(input: &[String]) -> HashMap<String, HashMap<String, i32>>
{
    let mut map: HashMap<String, HashMap<String, i32>> = HashMap::new();
    for line in input
    {
        let mut iter = line.split_whitespace();
        let src = iter.next().unwrap().to_string();
        let gain = iter.nth(1).unwrap() == "gain";
        let val: i32 = iter.next().unwrap().parse().unwrap();
        let dst = iter.last().unwrap();
        let dst = dst[..dst.len() - 1].to_string();

        let val = if gain { val } else { -val };
        map.entry(src.clone()).or_default().insert(dst.clone(), val);
    }
    map
}

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

fn solve(input: &[String], me: bool) -> i32
{
    let map = create_map(input);
    let mut keys: Vec<String> = map.keys().cloned().collect();
    if me
    {
        keys.push("me".into());
    }
    let perms = unique_permutations(keys);
    perms
        .into_iter()
        .map(|perm| {
            let mut happines = 0;
            for i in 0..perm.len() as isize
            {
                let next: usize = (i + 1).rem_euclid(perm.len() as isize) as usize;
                let prev: usize = (i - 1).rem_euclid(perm.len() as isize) as usize;

                let i = i as usize;
                let a = map.get(&perm[i]).map(|m2| m2.get(&perm[next]).unwrap_or(&0)).unwrap_or(&0);
                let b = map.get(&perm[i]).map(|m2| m2.get(&perm[prev]).unwrap_or(&0)).unwrap_or(&0);

                happines += a + b;
            }
            happines
        })
        .max()
        .unwrap()
}

fn task_one(input: &[String]) -> i32
{
    solve(input, false)
}

fn task_two(input: &[String]) -> i32
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
