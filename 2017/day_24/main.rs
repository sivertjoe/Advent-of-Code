use std::collections::*;

fn parse(input: &[String]) -> Vec<(i32, i32)>
{
    input
        .iter()
        .map(|line| {
            line.split_once('/')
                .map(|(a, b)| (a.parse::<i32>().unwrap(), b.parse().unwrap()))
                .unwrap()
        })
        .collect()
}

fn make_bridges(
    list: Vec<(i32, i32)>,
    available: Vec<(i32, i32)>,
    conn: i32,
    map: &mut HashMap<i32, Vec<Vec<(i32, i32)>>>,
)
{
    let mut flag = true;
    for (i, av) in available.iter().enumerate().filter(|(_i, (a, b))| *a == conn || *b == conn)
    {
        flag = false;
        let mut new_list = list.clone();
        new_list.push(*av);
        let new_conn = if av.0 == conn { av.1 } else { av.0 };
        let mut new_available = available.clone();
        new_available.swap_remove(i);

        make_bridges(new_list, new_available, new_conn, map);
    }

    if flag
    {
        let len = list.len() as i32;
        map.entry(len).or_insert(Vec::new()).push(list);
    }
}

fn task_one(input: &[String]) -> i32
{
    let input = parse(input);
    let mut map = HashMap::new();
    make_bridges(Vec::new(), input, 0, &mut map);

    map.into_values()
        .flatten()
        .map(|list| list.into_iter().map(|(a, b)| a + b).sum::<i32>())
        .max()
        .unwrap()
}

fn task_two(input: &[String]) -> i32
{
    let input = parse(input);
    let mut map = HashMap::new();
    make_bridges(Vec::new(), input, 0, &mut map);

    let largest = map.keys().max().unwrap();
    map[largest]
        .iter()
        .map(|list| list.into_iter().map(|(a, b)| a + b).sum::<i32>())
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
