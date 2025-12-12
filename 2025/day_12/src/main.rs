use ndarray::Array2;
use std::collections::*;

struct Mat(Array2<usize>);

struct Tree {
    shape: (usize, usize),
    idxs: Vec<usize>,
}

fn parse(input: &[String]) -> (HashMap<usize, Mat>, Vec<Tree>) {
    let p = input.iter().position(|line| line.contains('x')).unwrap();

    let mut map = HashMap::new();
    for mat in input[..p - 1].split(|line| line.is_empty()) {
        let idx = mat[0][..mat[0].len() - 1].parse::<usize>().unwrap();

        let vecs = mat[1..]
            .iter()
            .map(|line| {
                line.chars()
                    .map(|ch| if ch == '#' { 1_usize } else { 0 })
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<_>>();

        let array = Array2::from_shape_vec((3, 3), vecs).unwrap();
        let mat = Mat(array);
        map.insert(idx, mat);
    }

    let mut trees = Vec::new();
    for line in &input[p..] {
        let (fst, rest) = line.split_once(": ").unwrap();
        let (x, y) = fst.split_once('x').unwrap();
        let (x, y) = (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap());

        let rest = rest
            .split_whitespace()
            .map(|n| n.parse::<usize>().unwrap())
            .collect::<Vec<_>>();

        let tree = Tree {
            idxs: rest,
            shape: (x, y),
        };
        trees.push(tree);
    }

    (map, trees)
}

fn task_one(input: &[String]) -> usize {
    let (map, trees) = parse(input);
    let mut trees = trees;
    trees.sort_by_key(|key| key.shape.1);
    for t in trees.iter().take(5) {
        println!("{:?}", t.shape);
    }
    0
}

fn task_two(input: &[String]) -> usize {
    unimplemented!()
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
