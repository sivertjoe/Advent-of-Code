use std::collections::*;

fn split(line: &String) -> (&str, Vec<&str>)
{
    let (fst, snd) = line.split_once("->").unwrap();

    (
        fst.split_whitespace().next().unwrap(),
        snd.split_whitespace()
            .filter(|word| !word.is_empty())
            .map(|word| word.trim_matches(','))
            .collect(),
    )
}

#[derive(Debug)]
struct Tree
{
    name:     String,
    weight:   u32,
    children: Vec<String>,
}

fn create_tree(input: &[String]) -> HashMap<String, Tree>
{
    let mut map = HashMap::new();
    for node in input.into_iter().map(|line| {
        if line.contains("->")
        {
            let (node, children) = line.split_once("->").unwrap();
            let children = children
                .split_whitespace()
                .filter(|word| !word.is_empty())
                .map(|word| word.trim_matches(',').to_string())
                .collect();

            let (name, weight) = node.split_once(" ").unwrap();
            let name = name.to_string();
            let weight = weight[1..weight.len() - 2].parse::<u32>().unwrap();

            Tree {
                name,
                weight,
                children,
            }
        }
        else
        {
            let (name, weight) = line.split_once(" ").unwrap();
            let name = name.to_string();
            let weight = weight[1..weight.len() - 1].parse::<u32>().unwrap();
            Tree {
                name,
                weight,
                children: Vec::new(),
            }
        }
    })
    {
        map.insert(node.name.clone(), node);
    }

    map
}

fn find_root_node(input: &[String]) -> String
{
    let parents: Vec<(&str, Vec<&str>)> =
        input.iter().filter(|line| line.contains("->")).map(split).collect();
    let mut set = HashSet::new();
    for (parent, children) in &parents
    {
        set.insert(parent);
        for ch in children
        {
            set.insert(ch);
        }
    }
    for (_parent, children) in &parents
    {
        for ch in children
        {
            set.remove(ch);
        }
    }
    set.into_iter().next().unwrap().to_string()
}


fn task_one(input: &[String]) -> String
{
    find_root_node(input)
}

fn all_equal(arr: &[u32]) -> bool
{
    arr.windows(2).all(|w| w[0] == w[1])
}

fn weight(name: String, map: &HashMap<String, Tree>) -> u32
{
    let node = map.get(&name).unwrap();
    let mut sum = node.weight;
    for ch in &node.children
    {
        sum += weight(ch.into(), map);
    }
    sum
}

fn ws(node: &Tree, map: &HashMap<String, Tree>) -> Vec<u32>
{
    node.children.iter().map(|node| weight(node.into(), map)).collect()
}

fn unique(node: &Tree, ws: &[u32]) -> String
{
    for (i, v) in ws.iter().enumerate()
    {
        let count = ws.iter().filter(|n| *n == v).count();
        if count == 1
        {
            return node.children[i].clone();
        }
    }
    unreachable!()
}

fn diff(ws: &[u32]) -> i32
{
    for (i, v) in ws.iter().enumerate()
    {
        let count = ws.iter().filter(|n| *n == v).count();
        if count == 1
        {
            return *v as i32 - ws[(i + 1) % ws.len()] as i32;
        }
    }
    unreachable!()
}

fn rec<'map>(root: String, prev: Option<&'map Tree>, map: &'map HashMap<String, Tree>) -> u32
{
    let node = map.get(&root).unwrap();
    let weights = ws(&node, map);
    if all_equal(&weights)
    {
        let prev_weights = ws(prev.unwrap(), map);
        let diff = diff(&prev_weights);
        return (node.weight as i32 - diff) as u32;
    }
    else
    {
        let unique = unique(&node, &weights);
        rec(unique, Some(node), map)
    }
}

fn task_two(input: &[String]) -> u32
{
    let root = find_root_node(input);
    let tree = create_tree(input);
    rec(root, None, &tree)
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
