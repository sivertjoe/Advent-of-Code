use std::collections::*;

fn parse(input: &[String]) -> Vec<HashMap<String, usize>>
{
    let mut vec = Vec::new();
    for line in input
    {
        let mut map = HashMap::new();
        let (_, body) = line.split_once(": ").unwrap();
        for (k, v) in body.split(", ").map(|token| token.split_once(": ").unwrap())
        {
            map.insert(k.to_string(), v.parse().unwrap());
        }
        vec.push(map);
    }
    vec
}

fn task_one(input: &[String]) -> usize
{
    let items = [
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ];
    for (i, sue) in parse(input).into_iter().enumerate()
    {
        if items.iter().all(|item| {
            if let Some(count) = sue.get(item.0)
            {
                *count == item.1
            }
            else
            {
                true
            }
        })
        {
            return i + 1;
        }
    }
    0
}

fn task_two(input: &[String]) -> usize
{
    let items = [
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ];
    for (i, sue) in parse(input).into_iter().enumerate()
    {
        if items.iter().all(|item| {
            let normal = || if let Some(count) = sue.get(item.0) { *count == item.1 } else { true };
            let fewer = || if let Some(count) = sue.get(item.0) { *count < item.1 } else { true };
            let greater = || if let Some(count) = sue.get(item.0) { *count > item.1 } else { true };

            match item.0
            {
                "cats" | "trees" => greater(),
                "pomeranians" | "goldfish" => fewer(),
                _ => normal(),
            }
        })
        {
            return i + 1;
        }
    }
    0
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
