use std::collections::*;

fn parse(input: &[String]) -> HashMap<String, HashSet<String>> {
    let mut map: HashMap<String, HashSet<String>> = HashMap::new();

    for line in input {
        let (a, b) = line.split_once('-').unwrap();
        map.entry(a.to_string()).or_default().insert(b.to_string());
        map.entry(b.to_string()).or_default().insert(a.to_string());
    }
    map
}

fn check_all_connected(
    mut set: Vec<String>,
    mut vec: Vec<(String, HashSet<String>)>,
) -> Vec<String> {
    let clone = set.clone();

    for to_remove in clone {
        let pos = set.iter().position(|e| *e == to_remove).unwrap();
        set.swap_remove(pos);

        let pos = vec
            .iter()
            .position(|(name, _set)| *name == to_remove)
            .unwrap();

        let temp = vec.swap_remove(pos);

        if check_all(&set, &vec) {
            return set;
        }

        set.push(to_remove);
        vec.push(temp);
    }

    Vec::new()
}

fn check_all(set: &[String], vec: &[(String, HashSet<String>)]) -> bool {
    set.iter().all(|elem| {
        vec.iter()
            .filter(|(name, _set)| elem != name)
            .all(|(_name, set)| set.contains(elem))
    })
}

fn find_connected_group(
    k: &String,
    v: &HashSet<String>,
    map: &HashMap<String, HashSet<String>>,
) -> Vec<String> {
    let all = v.iter().cloned().collect::<Vec<_>>();

    let mut vec = Vec::new();
    for n in &all {
        let connected = map[n].clone();
        vec.push((n.clone(), connected));
    }

    let mut conn = check_all_connected(all, vec);
    conn.push(k.clone());

    conn
}

fn task_one(input: &[String]) -> usize {
    let map = parse(input);

    let mut triangles = HashSet::new();
    for line in input {
        let (a, b) = line.split_once('-').unwrap();
        let common = map[a].intersection(&map[b]);
        for w in common {
            let mut arr = [a, b, w];
            arr.sort();
            triangles.insert(arr);
        }
    }

    triangles
        .into_iter()
        .filter(|arr| arr.iter().any(|e| e.starts_with("t")))
        .count()
}

fn task_two(input: &[String]) -> String {
    let map = parse(input);

    let mut largest = map
        .iter()
        .map(|(k, v)| find_connected_group(k, v, &map))
        .max_by_key(|v| v.len())
        .unwrap();

    largest.sort();
    largest.join(",")
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
    time(Task::Two, task_two, &input);
}

fn read_input<P>(path: P) -> Vec<String>
where
    P: AsRef<std::path::Path>,
{
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

enum Task {
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
    let elapsed = t.elapsed();
    let fmt = std::env::var("TASKUNIT").unwrap_or("ms".to_owned());

    let (u, elapsed) = match fmt.as_str() {
        "ms" => ("ms", elapsed.as_millis()),
        "ns" => ("ns", elapsed.as_nanos()),
        "us" => ("μs", elapsed.as_micros()),
        "s" => ("s", elapsed.as_secs() as u128),
        _ => panic!("unsupported time format"),
    };

    match task {
        Task::One => {
            println!("({}{u})\tTask one: \x1b[0;34;34m{}\x1b[0m", elapsed, res);
        }
        Task::Two => {
            println!("({}{u})\tTask two: \x1b[0;33;10m{}\x1b[0m", elapsed, res);
        }
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
