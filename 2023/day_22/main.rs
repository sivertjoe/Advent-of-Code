use std::collections::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

use std::fmt;
impl fmt::Debug for Brick {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: [{:?} {:?}]", self.2 as u8 as char, self.0, self.1)
    }
}


#[derive(Clone, Copy, PartialEq, Eq)]
struct Brick(Point, Point, usize);

fn parse(input: &[String]) -> Vec<Brick> {
    let mut id: usize = b'A' as usize;
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
            let b = Brick(parse(a), parse(b), id);
            id += 1;
            b
        })
        .collect::<Vec<_>>()
}

fn is_solid(set: &HashSet<(isize, isize, isize)>, x: isize, y: isize, z: isize) -> bool {
    if z == 0 {
        true 
    } else {
        set.contains(&(x, y, z))
    }
}



fn fall(bricks: Vec<Brick>) -> (bool, Vec<Brick>) {
    let mut change = false;
    let mut set = HashSet::new();

    for brick in &bricks {
        for x in brick.0.x..=brick.1.x {
            for y in brick.0.y..=brick.1.y {
                set.insert((x, y, brick.1.z));
            }
        }
    }

    let mut new_bricks = Vec::new();

    for brick in bricks {
        let mut supp = false;
        'outer: for x in brick.0.x..=brick.1.x {
            for y in brick.0.y..=brick.1.y {
                if is_solid(&set, x, y, brick.0.z - 1) {
                    supp = true;
                    break 'outer;
                }
            }
        }

        if !supp {
            change = true;
            let mut cpy = brick.clone();
            cpy.0.z -= 1;
            cpy.1.z -= 1;
            new_bricks.push(cpy);
        } else {
            new_bricks.push(brick.clone());
        }
    }

    (change, new_bricks)
}

fn task_one(input: &[String]) -> isize {
    let mut bricks = parse(input);
    let mut sum = 0;

    let mut change = true;
    while change {
        (change, bricks) = fall(bricks);
    }

    for i in 0..bricks.len() {
        let mut tmp = bricks.clone();
        tmp.remove(i);
        if !fall(tmp).0 {
            sum += 1;
        }
    }
    sum
}

fn task_two(input: &[String]) -> isize {
    let mut bricks = parse(input);
    let mut sum = 0;

    let mut change = true;
    while change {
        (change, bricks) = fall(bricks);
    }

    for i in 0..bricks.len() {
        let mut copy = bricks.clone();
        copy.remove(i);
        
        let mut lchange = true;
        while lchange {
            (lchange, copy) = fall(copy);
        }
        let mut map = HashMap::new();
        for cpy in copy {
            map.insert(cpy.2, cpy.0.z);
        }

        for b in &bricks {
            let Some(z0) = map.get(&b.2) else { continue; };
            if b.0.z != *z0 {
                sum += 1;
            }
        }
    }
    sum

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
