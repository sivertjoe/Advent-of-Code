use std::collections::*;

fn parse(input: &[String]) -> (String, Vec<(String, String)>)
{
    let mut vec = Vec::new();

    let mut flag = false;
    for line in input
    {
        if flag
        {
            return (line.to_string(), vec);
        }
        if line.is_empty()
        {
            flag = true;
            continue;
        }
        else
        {
            let (k, v) = line.split_once(" => ").unwrap();
            let k = k.to_string();
            vec.push((k, v.to_string()));
        }
    }
    unreachable!()
}

fn task_one(input: &[String]) -> usize
{
    let (mol, replacements) = parse(input);
    let mut set = HashSet::new();
    for (src, repl) in replacements
    {
        for (i, _) in mol.match_indices(&src)
        {
            let new_s = format!("{}{}{}", &mol[..i], repl, &mol[i + src.len()..]);
            set.insert(new_s);
        }
    }
    set.len()
}

fn task_two(input: &[String]) -> usize
{
    // Tried to solve this with BFS, however, my computer blew up.
    // Key insight is that there is only one solution.
    // One way to get there is to repeatedly fold the first occurences
    // until we get to 'e'
    let (mut mol, replacements) = parse(input);
    let mut count = 0;
    while mol != "e"
    {
        for (src, repl) in replacements.iter()
        {
            if mol.contains(repl)
            {
                mol = mol.replacen(repl, src, 1);
                count += 1;
            }
        }
    }
    count
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
