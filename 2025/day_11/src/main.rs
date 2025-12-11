use std::collections::*;

use petgraph::graph::DiGraph;

fn parse(input: &[String]) -> HashMap<String, Vec<String>> {
    let mut map = HashMap::new();

    for line in input {
        let (key, rest) = line.split_once(": ").unwrap();
        let rest = rest
            .split_whitespace()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();

        map.insert(key.to_owned(), rest);
    }
    map
}

fn task_one(input: &[String]) -> usize {
    let map = parse(input);
    let start = "you".to_string();

    let mut count = 0;
    let mut vec = VecDeque::new();
    vec.push_back(&start);

    while let Some(curr) = vec.pop_front() {
        if curr == "out" {
            count += 1;
        } else {
            for next in &map[curr] {
                vec.push_back(next);
            }
        }
    }
    count
}

fn bfs(map: &HashMap<String, Vec<String>>, start: String, end: String) -> Vec<Vec<String>> {
    let mut vec = VecDeque::new();
    let mut paths = Vec::new();

    let mut seen = HashSet::new();
    seen.insert(start.clone());
    vec.push_back((start.clone(), seen));

    while let Some((curr, seen)) = vec.pop_front() {
        if curr == end {
            let p = seen.into_iter().collect::<Vec<_>>();
            paths.push(p);
        } else {
            if let Some(nexts) = map.get(&curr) {
                for next in nexts {
                    if !seen.contains(next) {
                        let mut new_seen = seen.clone();
                        new_seen.insert(next.clone());
                        vec.push_back((next.clone(), new_seen));
                    }
                }
            }
        }
    }
    paths
}

fn reverse(input: &[String]) -> HashMap<String, Vec<String>> {
    let mut map: HashMap<String, Vec<String>> = HashMap::new();

    for line in input {
        let (key, rest) = line.split_once(": ").unwrap();
        let rest = rest
            .split_whitespace()
            .map(|s| s.to_string())
            .collect::<Vec<_>>();

        for r in rest {
            map.entry(r).or_default().push(key.to_string());
        }
    }
    map
}

fn parse2(map: HashMap<String, Vec<String>>) -> DiGraph<String, String> {
    let mut graph = DiGraph::new();

    let mut nodes = HashMap::new();

    for (node, tos) in map {
        let a = *nodes
            .entry(node.clone())
            .or_insert_with(|| graph.add_node(node.clone()));

        for to in tos {
            let p = *nodes
                .entry(to.clone())
                .or_insert_with(|| graph.add_node(to.clone()));
            graph.add_edge(a, p, format!("{node}->{to}"));
        }
    }

    graph
}

fn task_two(input: &[String]) -> usize {
    let map = parse(input);

    let mut graph = DiGraph::new();

    let mut nodes = HashMap::new();

    for (node, tos) in map {
        let a = *nodes
            .entry(node.clone())
            .or_insert_with(|| graph.add_node(node.clone()));

        for to in tos {
            let p = *nodes
                .entry(to.clone())
                .or_insert_with(|| graph.add_node(to.clone()));
            graph.add_edge(a, p, 1);
        }
    }

    let from = "svr".to_string();
    let to = "dac".to_string();

    use petgraph::algo::all_simple_paths;
    use std::collections::hash_map::RandomState;

    let from = nodes[&from];
    let to = nodes[&to];

    let paths = all_simple_paths::<HashSet<_>, _, RandomState>(&graph, from, to, 0, None)
        .collect::<Vec<_>>();
    println!("done?");

    let fft = nodes["fft"];
    let dac = nodes["dac"];

    let mut count = 0;
    for path in paths {
        if path.contains(&fft) && path.contains(&dac) {
            count += 1;
        }
    }

    count
}

fn main() {
    let input = read_input(get_input_file());
    // time(Task::One, task_one, &input);
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
