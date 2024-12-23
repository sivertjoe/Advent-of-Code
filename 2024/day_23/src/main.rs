use petgraph::prelude::*;
use std::collections::*;

fn task_one(input: &[String]) -> usize {
    let mut map: HashMap<String, HashSet<String>> = HashMap::new();
    let mut edges = Vec::new();

    for line in input {
        let (a, b) = line.split_once('-').unwrap();
        map.entry(a.to_string()).or_default().insert(b.to_string());
        map.entry(b.to_string()).or_default().insert(a.to_string());

        edges.push((a.to_string(), b.to_string()));
    }

    let mut triangles = HashSet::new();
    for (a, b) in &edges {
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

fn find_connected_neighbors(
    graph: &Graph<String, (), Undirected>,
    index: NodeIndex,
) -> HashSet<NodeIndex> {
    // let neighbors: HashSet<NodeIndex<Ix>> = graph.neighbors(index).collect();
    let neighbors: HashSet<NodeIndex> = graph
        .neighbors_directed(index, Direction::Incoming)
        .collect();

    // Include the node itself
    let mut connected_neighbors = neighbors.clone();
    connected_neighbors.insert(index);

    // Create a new set for the result
    let mut result = HashSet::new();

    for &node in &connected_neighbors {
        // Get the neighbors of the current node
        let node_neighbors: HashSet<NodeIndex> = graph
            .neighbors_directed(node, Direction::Incoming)
            .collect();

        // Check if all other nodes in the group are in this node's neighbors
        if connected_neighbors.is_subset(&node_neighbors) {
            result.insert(node);
        }
    }

    result
}

fn check_all(set: &[String], vec: &[(String, HashSet<String>)]) -> bool {
    set.iter().all(|elem| {
        vec.iter()
            .filter(|(name, set)| elem != name)
            .all(|(name, set)| set.contains(elem))
    })
}

fn check_all_connected(
    mut set: Vec<String>,
    mut vec: Vec<(String, HashSet<String>)>,
) -> Vec<String> {
    // First check if all are connected
    if check_all(&set, &vec) {
        return set;
    }

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

    return Vec::new();
}

fn task_two(input: &[String]) -> String {
    let mut graph = Graph::new_undirected();
    let mut indexes = HashMap::new();
    let mut indexes_rev = HashMap::new();

    for line in input {
        let (a, b) = line.split_once('-').unwrap();
        let a = a.to_string();
        let b = b.to_string();

        if !indexes.contains_key(&a) {
            let ao = graph.add_node(a.to_string());
            indexes.insert(a.clone(), ao);
            indexes_rev.insert(ao, a.clone());
        }
        if !indexes.contains_key(&b) {
            let bo = graph.add_node(b.to_string());
            indexes.insert(b.clone(), bo);
            indexes_rev.insert(bo, b.clone());
        }

        let aa = indexes.get(&a).unwrap();
        let bb = indexes.get(&b).unwrap();
        graph.add_edge(*aa, *bb, ());
    }

    let mut largest = Vec::new();
    for index in indexes.values() {
        let connected: Vec<NodeIndex> = graph
            .neighbors_directed(*index, Direction::Incoming)
            .collect();

        let all = connected
            .iter()
            .map(|i| indexes_rev[i].clone())
            .collect::<Vec<_>>();

        let mut vec = Vec::new();
        for n in connected {
            let connected: Vec<NodeIndex> =
                graph.neighbors_directed(n, Direction::Incoming).collect();
            let all = connected
                .iter()
                .map(|i| indexes_rev[i].clone())
                .collect::<HashSet<_>>();
            vec.push((indexes_rev[&n].clone(), all));
        }

        let mut conn = check_all_connected(all, vec);
        conn.push(indexes_rev[index].clone());

        if conn.len() > largest.len() {
            largest = conn;
        }
    }
    largest.sort();
    let password = largest.join(",");
    password
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
