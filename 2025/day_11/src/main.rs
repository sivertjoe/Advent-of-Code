use std::collections::*;

use petgraph::{graph::DiGraph, matrix_graph::NodeIndex, visit::EdgeRef};

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

fn parse2(
    map: HashMap<String, Vec<String>>,
) -> (DiGraph<String, String>, HashMap<String, NodeIndex<u32>>) {
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

    (graph, nodes)
}

fn total_paths(levels: &Vec<Vec<&str>>, calc: impl Fn(&str, &str) -> usize) -> usize {
    // ways[node] = number of paths from "svr" to this node so far
    let mut ways: HashMap<&str, usize> = HashMap::new();
    ways.insert("svr", 1); // we start at "svr" with one path

    // iterate over consecutive pairs of levels: [L0,L1], [L1,L2], ...
    for window in levels.windows(2) {
        let cur_level = &window[0];
        let next_level = &window[1];

        let mut next_ways: HashMap<&str, usize> = HashMap::new();

        for &u in cur_level {
            if let Some(&count_u) = ways.get(u) {
                if count_u == 0 {
                    continue;
                }
                for &v in next_level {
                    let paths_uv = calc(u, v);
                    if paths_uv == 0 {
                        continue;
                    }
                    *next_ways.entry(v).or_insert(0) += count_u * paths_uv;
                }
            }
        }

        ways = next_ways;
    }

    // at the end, "out" is in the last level
    let goal = levels.last().unwrap()[0];
    *ways.get(goal).unwrap_or(&0)
}

use petgraph::algo::all_simple_paths;
use std::collections::hash_map::RandomState;

fn task_two(input: &[String]) -> usize {
    let map = parse(input);

    let (norm, norm_nodes) = parse2(map);

    let calc = |from: &str, to: &str| -> usize {
        all_simple_paths::<HashSet<_>, _, RandomState>(
            &norm,
            norm_nodes[from],
            norm_nodes[to],
            0,
            Some(6),
        )
        .count()
    };

    #[rustfmt::skip]
    let levels = vec![
        vec!["svr"],
        vec!["jmp", "ekk", "mwn", "iip"],
        vec!["fft"],
        vec!["wwr", "jeu", "dyb"],
        vec!["cok", "cyz", "eqi"],
        vec!["sxd", "krn", "jqy"],
        vec!["dac"],
        vec!["svi", "dpv", "you"],
        vec!["out"]
    ];

    return total_paths(&levels, calc);
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
