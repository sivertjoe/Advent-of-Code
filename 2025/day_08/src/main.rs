use std::{cmp::Reverse, collections::*};

#[derive(Eq, PartialEq, Hash, Clone, Debug, Copy)]
struct Point {
    x: usize,
    y: usize,
    z: usize,
}

impl Point {
    fn dist(&self, other: &Point) -> f32 {
        let inner = ((self.x as isize - other.x as isize).pow(2)
            + (self.y as isize - other.y as isize).pow(2)
            + (self.z as isize - other.z as isize).pow(2)) as f32;
        inner.sqrt()
    }
}

fn parse(input: &[String]) -> Vec<Point> {
    let mut points = Vec::new();
    for line in input {
        let mut spl = line.trim_end().split(',');

        points.push(Point {
            x: spl.next().unwrap().parse().unwrap(),
            y: spl.next().unwrap().parse().unwrap(),
            z: spl.next().unwrap().parse().unwrap(),
        });
    }
    points
}

fn task_one(input: &[String]) -> usize {
    let points = parse(input);

    let mut dists = HashMap::new();

    for i in 0..points.len() {
        for j in i + 1..points.len() {
            if i == j {
                continue;
            }
            let dist = points[i].dist(&points[j]);
            dists.insert((i, j), dist);
        }
    }

    let mut dists2 = dists.into_iter().collect::<Vec<_>>();
    dists2.sort_by(|a, b| a.1.total_cmp(&b.1));

    let mut circuts: Vec<HashSet<Point>> = Vec::new();

    for ((a, b), _dist) in dists2.into_iter().take(1000) {
        let ap = points[a].clone();
        let bp = points[b].clone();

        let mut found_a: Option<usize> = None;
        let mut found_b: Option<usize> = None;
        for (i, c) in circuts.iter_mut().enumerate() {
            if c.contains(&ap) {
                found_a = Some(i);
            } else if c.contains(&bp) {
                found_b = Some(i);
            }
        }

        match (found_a, found_b) {
            (Some(i), Some(j)) => {
                let (max, min) = (std::cmp::max(i, j), std::cmp::min(i, j));
                let set1 = circuts.swap_remove(max);
                let set2 = circuts.swap_remove(min);

                let union = set1.union(&set2).cloned().collect::<HashSet<Point>>();
                circuts.push(union);
            }
            (Some(i), None) => {
                circuts[i].insert(bp);
            }
            (None, Some(j)) => {
                circuts[j].insert(ap);
            }
            (None, None) => {
                let mut new_circut = HashSet::new();
                new_circut.insert(ap);
                new_circut.insert(bp);
                circuts.push(new_circut);
            }
        }
    }

    circuts.sort_by_key(|set| Reverse(set.len()));
    circuts.into_iter().map(|set| set.len()).take(3).product()
}

fn task_two(input: &[String]) -> usize {
    let points = parse(input);

    let mut dists = HashMap::new();

    for i in 0..points.len() {
        for j in i + 1..points.len() {
            if i == j {
                continue;
            }
            let dist = points[i].dist(&points[j]);
            dists.insert((i, j), dist);
        }
    }

    let mut dists2 = dists.into_iter().collect::<Vec<_>>();
    dists2.sort_by(|a, b| a.1.total_cmp(&b.1));

    let mut circuts: Vec<HashSet<Point>> = Vec::new();

    for ((a, b), _dist) in dists2 {
        let ap = points[a].clone();
        let bp = points[b].clone();

        let mut found_a: Option<usize> = None;
        let mut found_b: Option<usize> = None;
        for (i, c) in circuts.iter_mut().enumerate() {
            if c.contains(&ap) {
                found_a = Some(i);
            } else if c.contains(&bp) {
                found_b = Some(i);
            }
        }

        match (found_a, found_b) {
            (Some(i), Some(j)) => {
                let (max, min) = (std::cmp::max(i, j), std::cmp::min(i, j));
                let set1 = circuts.swap_remove(max);
                let set2 = circuts.swap_remove(min);

                let union = set1.union(&set2).cloned().collect::<HashSet<Point>>();
                circuts.push(union);
            }
            (Some(i), None) => {
                circuts[i].insert(bp);
            }
            (None, Some(j)) => {
                circuts[j].insert(ap);
            }
            (None, None) => {
                let mut new_circut = HashSet::new();
                new_circut.insert(ap);
                new_circut.insert(bp);
                circuts.push(new_circut);
            }
        }

        if circuts.len() == 1 && circuts.iter().next().unwrap().len() == points.len() {
            return ap.x * bp.x;
        }
    }

    return 0;
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
