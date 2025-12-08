use std::collections::*;

#[derive(Eq, PartialEq, Hash, Clone)]
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

    fn print(&self) {
        println!("X={}, Y={}, Z={}", self.x, self.y, self.z);
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
            let dist = points[i].dist(&points[j]);
            dists.insert((i, j), dist);
        }
    }

    let mut circuts: Vec<HashSet<Point>> = Vec::new();

    for _ in 0..10 {
        let ((a, b), _dist) = dists.iter().min_by(|a, b| a.1.total_cmp(b.1)).unwrap();

        let ap = points[*a].clone();
        let bp = points[*b].clone();

        ap.print();
        bp.print();

        let mut found = false;
        for c in &mut circuts {
            if c.contains(&ap) {
                c.insert(bp.clone());
                found = true;
            }
            if c.contains(&bp) {
                c.insert(ap.clone());
                found = true;
            }
        }

        if !found {
            let mut set = HashSet::new();
            set.insert(ap);
            set.insert(bp);
            circuts.push(set);
        }
        dists.remove(&(*a, *b));
        for c in &circuts {
            print!("{} ", c.len());
        }
        println!();
    }

    for c in circuts {
        println!("{}", c.len());
    }

    return 0;
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
