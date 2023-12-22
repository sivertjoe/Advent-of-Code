use std::collections::*;

#[derive(Clone, Copy, PartialEq, Eq)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Brick(Point, Point);
impl Brick {
    fn lowest_z(&self) -> isize {
        if self.0.z < self.1.z {
            self.0.z
        } else {
            self.1.z
        }
    }
    fn get_ys(&self) -> (isize, isize) {
        if self.0.y < self.1.y {
            (self.0.y, self.1.y)
        } else {
            (self.1.y, self.0.y)
        }
    }
    fn get_xs(&self) -> (isize, isize) {
        if self.0.x < self.1.x {
            (self.0.x, self.1.x)
        } else {
            (self.1.x, self.0.x)
        }
    }

    fn overlaps(&self, p: &Brick) -> bool {
        let (st, ed) = self.get_ys();
        let (st2, ed2) = p.get_ys();
        false
    }
}

fn parse(input: &[String]) -> Vec<Brick> {
    let p = |s: Option<&str>| -> isize { s.unwrap().parse::<isize>().unwrap() };

    let parse = |s: &str| {
        let mut iter = s.split(',');
        Point {
            x: p(iter.next()),
            y: p(iter.next()),
            z: p(iter.next()),
        }
    };
    input
        .iter()
        .map(|line| {
            let (a, b) = line.split_once('~').unwrap();
            Brick(parse(a), parse(b))
        })
        .collect::<Vec<_>>()
}

fn find_overlapping_bricks_above(brick: Brick, bricks: &[Brick]) -> Vec<Brick> {
    let mut vec = Vec::new();

    for b in bricks {
        if brick == *b {
            continue;
        }
        if b.lowest_z() >= brick.lowest_z() {
            continue;
        }
    }

    vec
}

fn bricks_overlap(brick1: &Brick, brick2: &Brick) -> bool {
    let a1 = brick1.0;
    let b1 = brick1.1;
    let a2 = brick2.0;
    let b2 = brick2.1;

    let denominator = (b1.x - a1.x) * (b2.y - a2.y) - (b1.y - a1.y) * (b2.x - a2.x);

    if denominator == 0 {
        // Lines are parallel or coincident
        return false;
    }

    let t = ((a2.x - a1.x) * (b2.y - a2.y) - (a2.y - a1.y) * (b2.x - a2.x)) / denominator;
    let s = ((a2.x - a1.x) * (b1.y - a1.y) - (a2.y - a1.y) * (b1.x - a1.x)) / denominator;

    // Check if the values of t and s are within the valid range (0 to denominator)
    t >= 0 && t <= denominator && s >= 0 && s <= denominator
}

fn collides_with_other(brick: &Brick, bricks: &[Brick]) -> bool {
    for b in bricks {
        if b == brick { continue; }
        if bricks_overlap(brick, b) {
            return true;
        }
    }
    false
}


fn fall(bricks: &mut [Brick]) {
    loop {
        let mut change = false;
        for i in 0..bricks.len() {
            let cpy = bricks[i];

            while cpy.lowest_z() > 0 && 
        }

        if !change {
            break;
        }
    }
}

fn task_one(input: &[String]) -> isize {
    let bricks = parse(input);
    let mut sum = 0;

    for brick in &bricks {}
    sum
}

fn task_two(input: &[String]) -> isize {
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
