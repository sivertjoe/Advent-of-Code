use std::collections::*;

use petgraph::prelude::*;
use rustworkx_core::connectivity::stoer_wagner_min_cut;

fn parse(input: &[String]) -> HashMap<&str, Vec<&str>> {
    input
        .iter()
        .map(|l| {
            let (fst, snd) = l.split_once(": ").unwrap();
            (fst, snd.split_whitespace().collect())
        })
        .collect()
}

fn task_one(input: &[String]) -> usize {
    let modules = parse(input);
    let mut graph: Graph<&str, (), Undirected, u32> = Graph::default();

    let mut nodes: HashMap<_, _> = modules
        .keys()
        .map(|name| (*name, graph.add_node(*name)))
        .collect();

    for name in modules.values().flat_map(|v| v.iter()) {
        if !nodes.contains_key(name) {
            let node = graph.add_node(name);
            nodes.insert(name, node);
        }
    }

    for (&src, dsts) in &modules {
        for d in dsts {
            graph.add_edge(nodes[src], nodes[d], ());
        }
    }

    let (_len, nodes) = stoer_wagner_min_cut(&graph, |_| Result::<usize, ()>::Ok(1))
        .unwrap()
        .unwrap();

    nodes.len() * (graph.node_count() - nodes.len())
}

fn main() {
    let input = read_input(get_input_file());
    time(Task::One, task_one, &input);
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
    };
}

fn get_input_file() -> String {
    std::env::args()
        .nth(1)
        .unwrap_or_else(|| "input".to_string())
}
