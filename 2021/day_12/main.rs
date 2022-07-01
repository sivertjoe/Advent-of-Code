use std::collections::*;


fn read_input<P>(path: P) -> (Vec<String>, HashMap<String, Vec<String>>)
where
    P: AsRef<std::path::Path>,
{
    use std::io::BufRead;
    let file = std::fs::File::open(path).unwrap();
    // Separate start points from non-start points
    let (starts, not_starts): (Vec<(String, String)>, Vec<(String, String)>) =
        std::io::BufReader::new(file)
            .lines()
            .flatten()
            .map(|line| line.split_once('-').map(|(a, b)| (a.to_string(), b.to_string())).unwrap())
            .partition(|(s, e)| (s.starts_with("start") || e.starts_with("start")));

    let mut map = HashMap::new();
    for (a, b) in not_starts
    {
        let entry = map.entry(a.clone()).or_insert_with(Vec::new);
        entry.push(b.clone());

        let entry = map.entry(b.clone()).or_insert_with(Vec::new);
        entry.push(a.clone());
    }

    let points_from_start = starts
        .into_iter()
        .map(|(a, b)| if a.starts_with("start") { b } else { a })
        .collect();

    (points_from_start, map)
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

fn main()
{
    let input = read_input("input");
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn solve_task((starts, map): &(Vec<String>, HashMap<String, Vec<String>>), flag: bool) -> i32
{
    let mut paths = 0;

    for node in starts
    {
        let mut set = HashSet::new();
        set.insert(node.as_str());
        search(map, &mut paths, set, flag, node);
    }

    paths
}

fn task_one(input: &(Vec<String>, HashMap<String, Vec<String>>)) -> i32
{
    solve_task(input, true)
}

fn task_two(input: &(Vec<String>, HashMap<String, Vec<String>>)) -> i32
{
    solve_task(input, false)
}

#[inline]
fn small_cave(s: &str) -> bool
{
    s.chars().all(char::is_lowercase)
}

fn search(
    map: &HashMap<String, Vec<String>>,
    paths: &mut i32,
    seen: HashSet<&str>,
    seen_twice: bool,
    current: &str,
)
{
    for node in &map[&current.to_string()]
    {
        if node == "end"
        {
            *paths += 1;
            continue;
        }

        if small_cave(node)
        {
            if !seen_twice || !seen.contains(node.as_str())
            {
                let mut seen = seen.clone();
                let seen_twice = !seen.insert(node.as_str()) || seen_twice;
                search(map, paths, seen, seen_twice, node);
            }
        }
        else
        {
            search(map, paths, seen.clone(), seen_twice, node);
        }
    }
}
