use std::collections::*;

type P = (isize, isize);

fn manhatten_distance(a: P, b: P) -> usize {
    a.0.abs_diff(b.0) + a.1.abs_diff(b.1)
}

/*
    fn in_line(a: P, b: P) -> Option<((isize, isize), (isize, isize))> {
    if a == b {
        return None;
    }
    let res = if a.0 == b.0 {
        let dist = a.1.abs_diff(b.1) as isize;
        if a.1 > b.1 {
            Some(((a.0, a.1 + dist), (b.0, b.1 - dist)))
        } else {
            Some(((a.0, a.1 - dist), (b.0, b.1 + dist)))
        }
    } else if a.1 == b.1 {
        let dist = a.0.abs_diff(b.0) as isize;
        if a.0 > b.0 {
            Some(((a.0 + dist, a.1), (b.0 - dist, b.1)))
        } else {
            Some(((a.0 - dist, a.1), (b.0 + dist, b.1)))
        }
    } else if a.0.abs_diff(b.0) == a.1.abs_diff(b.1) {
        let disty = a.0.abs_diff(b.0) as isize;
        let distx = a.1.abs_diff(b.1) as isize;

        if a.0 < b.0 && a.1 < b.1 {
            Some(((a.0 - disty, a.1 - distx), (b.0 + disty, b.1 + distx)))
        } else if a.0 < b.0 && a.1 > b.1 {
            Some(((a.0 - disty, a.1 + distx), (b.0 + disty, b.1 - distx)))
        } else if a.0 > b.0 && a.1 < b.1 {
            Some(((a.0 + disty, a.1 - distx), (b.0 - disty, b.1 + distx)))
        } else if a.0 > b.0 && a.1 > b.1 {
            Some(((a.0 + disty, a.1 + distx), (b.0 - disty, b.1 - distx)))
        } else {
            unreachable!()
        }
    } else {
        None
    };
    res
}*/

fn in_line(a: P, b: P) -> Option<((isize, isize), (isize, isize))> {
    if a == b {
        return None;
    }
    let disty = a.0.abs_diff(b.0) as isize;
    let distx = a.1.abs_diff(b.1) as isize;

    if a.0 < b.0 && a.1 < b.1 {
        Some(((a.0 - disty, a.1 - distx), (b.0 + disty, b.1 + distx)))
    } else if a.0 < b.0 && a.1 > b.1 {
        Some(((a.0 - disty, a.1 + distx), (b.0 + disty, b.1 - distx)))
    } else if a.0 > b.0 && a.1 < b.1 {
        Some(((a.0 + disty, a.1 - distx), (b.0 - disty, b.1 + distx)))
    } else if a.0 > b.0 && a.1 > b.1 {
        Some(((a.0 + disty, a.1 + distx), (b.0 - disty, b.1 - distx)))
    } else {
        unreachable!()
    }
}

fn print_map(map: &HashMap<(isize, isize), char>) {
    let max_y = map.keys().map(|(y, _x)| y).max().unwrap();
    let max_x = map.keys().map(|(_y, x)| x).max().unwrap();

    for y in 0..=*max_y {
        for x in 0..=*max_x {
            if let Some(ch) = map.get(&(y, x)) {
                print!("{}", ch);
            } else {
                print!(",");
            }
        }
        println!();
    }
    println!();
}

fn task_one(input: &[String]) -> usize {
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            let y = y as isize;
            let x = x as isize;
            map.insert((y, x), ch);
        }
    }
    //print_map(&map);

    let mut set = HashSet::new();
    for (pos, ch) in map.iter() {
        for (p, c) in map.iter() {
            if *ch != *c || *ch == '.' || *ch == '#' {
                continue;
            }

            if let Some((a, b)) = in_line(*pos, *p) {
                set.insert(a);
                set.insert(b);
            }
        }
    }

    let max_y = *map.keys().map(|(y, _x)| y).max().unwrap();
    let max_x = *map.keys().map(|(_y, x)| x).max().unwrap();

    for (y, x) in set {
        if y >= 0 && y <= max_y && x >= 0 && x <= max_x {
            let elem = map.get_mut(&(y, x)).unwrap();
            //if *elem == '.' || *elem == '#' {
            *elem = 'x';
            //}
        }
    }
    //print_map(&map);

    map.values().filter(|ch| **ch == 'x').count()
}

fn in_line2(a: P, b: P, i: isize) -> Option<((isize, isize), (isize, isize))> {
    if a == b {
        println!("WHAT");
        return None;
    }
    let disty = i * (a.0.abs_diff(b.0) as isize);
    let distx = i * (a.1.abs_diff(b.1) as isize);

    if a.0 < b.0 && a.1 < b.1 {
        Some(((a.0 - disty, a.1 - distx), (b.0 + disty, b.1 + distx)))
    } else if a.0 < b.0 && a.1 > b.1 {
        Some(((a.0 - disty, a.1 + distx), (b.0 + disty, b.1 - distx)))
    } else if a.0 > b.0 && a.1 < b.1 {
        Some(((a.0 + disty, a.1 - distx), (b.0 - disty, b.1 + distx)))
    } else if a.0 > b.0 && a.1 > b.1 {
        Some(((a.0 + disty, a.1 + distx), (b.0 - disty, b.1 - distx)))
    } else {
        println!("??? {:?} {:?}", a, b);
        None
    }
}

fn run(map: HashMap<(isize, isize), char>) -> HashMap<(isize, isize), char> {
    let mut map = map;
    let mut set = HashSet::new();
    let max_y = *map.keys().map(|(y, _x)| y).max().unwrap();
    let max_x = *map.keys().map(|(_y, x)| x).max().unwrap();

    for (pos, ch) in map.iter() {
        for (p, c) in map.iter() {
            if *ch != *c || *ch == '.' || *ch == '#' || pos == p {
                continue;
            }

            for i in 1.. {
                let (a, b) = in_line2(*pos, *p, i).unwrap();
                let mut fin = false;
                if a.0 >= 0 && a.0 <= max_y && a.1 >= 0 && a.1 <= max_x {
                    set.insert(a);
                    fin = true;
                }
                if b.0 >= 0 && b.0 <= max_y && b.1 >= 0 && b.1 <= max_x {
                    set.insert(b);
                    fin = true;
                }

                if !fin {
                    break;
                }
            }
        }
    }

    for (y, x) in set {
        if y >= 0 && y <= max_y && x >= 0 && x <= max_x {
            let elem = map.get_mut(&(y, x)).unwrap();
            //if *elem == '.' || *elem == '#' {
            *elem = 'x';
            //}
        }
    }
    //print_map(&map);

    map
}

fn task_two(input: &[String]) -> usize {
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            let y = y as isize;
            let x = x as isize;
            map.insert((y, x), ch);
        }
    }

    map = run(map);

    map.values().filter(|ch| **ch != '.').count()
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
