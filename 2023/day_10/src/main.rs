type HashSet<K> = std::collections::HashSet<K, fxhash::FxBuildHasher>;

fn parse(input: &[String]) -> Vec<Vec<u8>> {
    input.iter().map(|line| line.as_bytes().to_vec()).collect()
}

fn start(vec: &[Vec<u8>]) -> (usize, usize) {
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            if vec[y][x] == b'S' {
                return (x, y);
            }
        }
    }
    unreachable!()
}

fn get_start_character(vec: &[Vec<u8>], pos: (usize, usize)) -> u8 {
    let my = vec.len() as isize;
    let mx = vec[0].len() as isize;
    let x = pos.0 as isize;
    let y = pos.1 as isize;

    let bounds = |tx, ty| tx >= 0 && tx < mx && ty >= 0 && ty < my;

    if bounds(x, y - 1)
        && bounds(x, y + 1)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'|', b'L', b'J'].contains(&vec[pos.1 + 1][pos.0])
    {
        return b'|';
    }
    if bounds(x - 1, y)
        && bounds(x + 1, y)
        && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'-';
    }
    if bounds(x, y - 1)
        && bounds(x + 1, y)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'L';
    }
    if bounds(x, y - 1)
        && bounds(x - 1, y)
        && [b'|', b'7', b'F'].contains(&vec[pos.1 - 1][pos.0])
        && [b'-', b'L', b'F'].contains(&vec[pos.1][pos.0 - 1])
    {
        return b'J';
    }
    if bounds(x, y + 1)
        && bounds(x + 1, y)
        && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0])
        && [b'-', b'J', b'7'].contains(&vec[pos.1][pos.0 + 1])
    {
        return b'F';
    }

    if bounds(x - 1, y)
        && bounds(x, y + 1)
        && [b'-', b'F', b'L'].contains(&vec[pos.1][pos.0 - 1])
        && [b'|', b'J', b'L'].contains(&vec[pos.1 + 1][pos.0])
    {
        return b'7';
    }
    unreachable!()
}

fn neighbors(vec: &[Vec<u8>], pos: (usize, usize)) -> ((usize, usize), (usize, usize)) {
    let me = vec[pos.1][pos.0];

    match me {
        b'|' => ((pos.0, pos.1 - 1), (pos.0, pos.1 + 1)),
        b'-' => ((pos.0 - 1, pos.1), (pos.0 + 1, pos.1)),
        b'L' => ((pos.0, pos.1 - 1), (pos.0 + 1, pos.1)),
        b'J' => ((pos.0, pos.1 - 1), (pos.0 - 1, pos.1)),
        b'7' => ((pos.0 - 1, pos.1), (pos.0, pos.1 + 1)),
        b'F' => ((pos.0, pos.1 + 1), (pos.0 + 1, pos.1)),
        _ => unreachable!(),
    }
}

fn get_map(vec: &[Vec<u8>], start: (usize, usize)) -> HashSet<(usize, usize)> {
    let mut seen = std::collections::HashSet::with_capacity_and_hasher(
        14000,
        fxhash::FxBuildHasher::default(),
    );
    let mut stack = vec![start];
    while let Some(pos) = stack.pop() {
        let (n1, n2) = neighbors(vec, pos);
        if seen.insert(n1) {
            stack.push(n1);
        }
        if seen.insert(n2) {
            stack.push(n2);
        }
    }
    seen
}

fn is_point_inside(
    vec: &[Vec<u8>],
    pos: (usize, usize),
    map: &HashSet<(usize, usize)>,
    go_left: bool,
) -> bool {
    let mut count = 0;
    let mut open_down = false;
    let mut open_up = false;

    let range = if go_left {
        0..pos.0
    } else {
        pos.0 + 1..vec[0].len()
    };

    for x in range {
        if !map.contains(&(x, pos.1)) {
            continue;
        }

        match vec[pos.1][x] {
            b'|' => count += 1,
            b'F' => open_down = true,
            b'L' => open_up = true,
            b'7' => {
                count += if open_down { 2 } else { 1 };
                open_up = false;
                open_down = false;
            }
            b'J' => {
                count += if open_up { 2 } else { 1 };
                open_up = false;
                open_down = false;
            }
            _ => {}
        }
    }

    count % 2 == 1
}

fn get_map_and_vec(input: &[String]) -> (HashSet<(usize, usize)>, Vec<Vec<u8>>) {
    let mut vec = parse(input);
    let start = start(&vec);
    let ch = get_start_character(&vec, start);
    vec[start.1][start.0] = ch;
    (get_map(&vec, start), vec)
}

fn task_one(input: &[String]) -> usize {
    let (map, _) = get_map_and_vec(input);
    map.len() / 2
}

fn task_two(input: &[String]) -> usize {
    let (map, vec) = get_map_and_vec(input);

    let mut count = 0;
    for y in 0..vec.len() {
        for x in 0..vec[0].len() {
            // Small optimization: go to the closest side
            let go_left = x < vec[0].len() / 2;
            if !map.contains(&(x, y)) && is_point_inside(&vec, (x, y), &map, go_left) {
                count += 1;
            }
        }
    }
    count
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
