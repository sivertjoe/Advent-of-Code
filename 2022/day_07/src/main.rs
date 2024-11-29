use std::{
    collections::*,
    path::{Path, PathBuf},
};

enum Command
{
    Cd(String),
    File(usize, String),
    Dir(String),
}

fn parse_line(line: &str) -> Option<Command>
{
    let mut spl = line.split_whitespace();
    let fst = spl.next().unwrap();
    if fst == "$"
    {
        let next = spl.next().unwrap();
        if next == "ls"
        {
            None // just ignore the ls value, it is not needed
        }
        else
        {
            let path = spl.next().unwrap().to_string();
            Some(Command::Cd(path))
        }
    }
    else
    {
        let path = spl.next().unwrap().to_string();
        if fst.starts_with("dir")
        {
            Some(Command::Dir(path))
        }
        else
        {
            let size = fst.parse::<usize>().unwrap();
            Some(Command::File(size, path))
        }
    }
}

enum Node
{
    File(usize),
    Dir(Vec<PathBuf>),
}

fn parse(input: &[String]) -> HashMap<PathBuf, Node>
{
    let mut map = HashMap::new();
    let mut curr: PathBuf = "/".into();
    map.insert(curr.clone(), Node::Dir(Vec::new()));
    for cmd in input.iter().skip(1).flat_map(|s| parse_line(s))
    {
        match cmd
        {
            Command::Cd(path) =>
            {
                if path == ".."
                {
                    curr.pop();
                }
                else if path == "/"
                {
                    curr = "/".into();
                }
                else
                {
                    curr.push(path);
                }
            },
            Command::File(size, name) =>
            {
                let folder = map.get_mut(&curr).unwrap();
                let Node::Dir(ref mut children) = folder else { panic!() };
                children.push(curr.join(&name));

                map.insert(curr.join(name), Node::File(size));
            },
            Command::Dir(name) =>
            {
                let folder = map.get_mut(&curr).unwrap();
                let Node::Dir(ref mut children) = folder else { panic!() };
                children.push(curr.join(&name));

                map.insert(curr.join(name), Node::Dir(Vec::new()));
            },
        };
    }
    map
}

fn size(path: &Path, map: &HashMap<PathBuf, Node>) -> usize
{
    match map.get(path).unwrap()
    {
        Node::File(size) => *size,
        Node::Dir(children) => children.iter().map(|d| size(d, map)).sum(),
    }
}

fn folder_sizes(map: &HashMap<PathBuf, Node>) -> impl Iterator<Item = usize> + '_
{
    map.iter().filter_map(move |(path, node)| match node
    {
        Node::File(_) => None,
        Node::Dir(_) => Some(size(path, map)),
    })
}

fn task_one(input: &[String]) -> usize
{
    let map = parse(input);
    folder_sizes(&map).filter(|size| *size < 100000).sum()
}

fn task_two(input: &[String]) -> usize
{
    let map = parse(input);
    const FS_SIZE: usize = 70000000;
    const FREE_SPACE: usize = 30000000;

    let mut sizes = folder_sizes(&map).collect::<Vec<_>>();
    sizes.sort_unstable();
    let used_space = *sizes.last().unwrap();

    let require = FREE_SPACE - (FS_SIZE - used_space);

    sizes.into_iter().find(|d| *d >= require).unwrap()
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
