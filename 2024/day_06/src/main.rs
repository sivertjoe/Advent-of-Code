use std::collections::*;

fn p(n: (isize, isize)) -> (isize, isize) {
    match n {
        (-1, 0) => (0, 1),
        (0, 1) => (1, 0),
        (1, 0) => (0, -1),
        (0, -1) => (-1, 0),
        _ => unreachable!(),
    }
}

fn get_char(inc: (isize, isize)) -> char {
    match inc {
        (-1, 0) => '^',
        (0, 1) => '>',
        (1, 0) => 'v',
        (0, -1) => '<',
        _ => unreachable!(),
    }
}

fn print_path(map: &HashMap<(isize, isize), char>, pos: (isize, isize), inc: (isize, isize)) {
    let max_y = map.keys().map(|(y, x)| *y).max().unwrap();
    let max_x = map.keys().map(|(y, x)| *x).max().unwrap();

    print!(" ");
    for i in 0..=max_x {
        print!("{i}");
    }
    println!();
    for y in 0..=max_y {
        print!("{y}");
        for x in 0..=max_x {
            if pos == (y, x) {
                print!("{}", get_char(inc));
            } else {
                print!("{}", map.get(&(y, x)).unwrap());
            }
        }
        println!();
    }
    println!();

    let mut _buf = String::new();
    let _ = std::io::stdin().read_line(&mut _buf);
}

fn next(pos: (isize, isize), inc: (isize, isize)) -> (isize, isize) {
    (pos.0 + inc.0, pos.1 + inc.1)
}

fn will_lead_to_loop(
    pos: (isize, isize),
    inc: (isize, isize),
    map: &HashMap<(isize, isize), char>,
    seen: &HashSet<((isize, isize), (isize, isize))>,
    ans: &HashSet<(isize, isize)>,
) -> Option<(isize, isize)> {
    /*let s = match ninc {
        (-1, 0) | (1, 0) => seen
            .iter()
            .find(|(_pos, _inc)| *_inc == ninc && _pos.0 == pos.0),
        (0, -1) | (0, 1) => seen
            .iter()
            .find(|(_pos, _inc)| *_inc == ninc && _pos.1 == pos.1),

        _ => unreachable!(),
    };
    if let Some(maybe) = s {
        if !ans.contains(&maybe.0) {
            Some(maybe.0)
        } else {
            None
        }
    } else {
        None
    }*/

    let opos = pos;
    let ninc = p(inc);
    let mut pos = pos;
    loop {
        let next = next(pos, ninc);
        match map.get(&next) {
            None => {
                return None;
            }
            Some(ch) => match ch {
                '#' => {
                    return None;
                }
                '.' => {
                    if seen.contains(&(pos, ninc)) {
                        return Some(pos);
                    }
                    pos = next;
                }
                _ => unreachable!(),
            },
        }
    }
}

fn explore_map(
    map: &HashMap<(isize, isize), char>,
    pos: (isize, isize),
    inc: (isize, isize),
) -> Option<usize> {
    let mut seen = HashSet::new();

    let mut pos = pos;
    let mut inc = inc;

    seen.insert((pos, inc));

    loop {
        let next = next(pos, inc);
        match map.get(&next) {
            None => {
                return Some(
                    seen.into_iter()
                        .map(|(pos, _inc)| pos)
                        .collect::<HashSet<_>>()
                        .len(),
                )
            }
            Some(ch) if *ch == '#' => {
                inc = p(inc);
            }
            Some(ch) if *ch == '.' => {
                // loop
                if !seen.insert((next, inc)) {
                    return None;
                }
                pos = next;
            }

            _ => unreachable!(),
        }
    }
}

fn task_one(input: &[String]) -> usize {
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            map.insert((y as isize, x as isize), ch);
        }
    }
    let pos = { *map.iter().find(|(_k, v)| **v == '^').map(|e| e.0).unwrap() };
    let inc = (-1, 0);
    *map.get_mut(&pos).unwrap() = '.';

    explore_map(&map, pos, inc).unwrap()
}

fn task_two(input: &[String]) -> usize {
    let mut map = HashMap::new();
    for (y, line) in input.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            map.insert((y as isize, x as isize), ch);
        }
    }
    let pos = { *map.iter().find(|(_k, v)| **v == '^').map(|e| e.0).unwrap() };
    let inc = (-1, 0);
    *map.get_mut(&pos).unwrap() = '.';

    let tiles = map
        .iter()
        .filter_map(|(k, v)| (*v == '.').then_some(*k))
        .collect::<Vec<_>>();

    let mut sum = 0;
    for tile in tiles {
        *map.get_mut(&tile).unwrap() = '#';
        if explore_map(&map, pos, inc).is_none() {
            sum += 1;
        }
        *map.get_mut(&tile).unwrap() = '.';
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
