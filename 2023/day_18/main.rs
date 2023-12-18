use std::collections::*;

fn task_one(input: &[String]) -> usize {
    //let mut map = HashMap::new();
    let mut set = HashSet::new();

    let mut curr: (isize, isize) = (0, 0);

    let mut start_x = 0;
    let mut start_y = 0;

    for line in input.iter() {
        let mut iter = line.split_ascii_whitespace();
        let dir = iter.next().unwrap();
        let num = iter.next().unwrap().parse::<usize>().unwrap();

        let (dy, dx) = match dir {
            "U" => (-1, 0),
            "D" => (1, 0),
            "R" => (0, 1),
            "L" => (0, -1),
            _ => unreachable!(),
        };

        for _ in 0..num {
            set.insert(curr);
            curr.0 += dy;
            curr.1 += dx;
        }
    }

    let mut start = (1, 4);
    let mut stack = vec![start];
    let mut seen = HashSet::new();
    seen.insert(start);

    while let Some(p) = stack.pop() {
        for n in [(-1, 0), (1, 0), (0, 1), (0, -1)] {
            let next = (p.0 + n.0, p.1 + n.1);
            if !set.contains(&next) && seen.insert(next) {
                stack.push(next);
            }
        }
    }

    seen.len() + set.len()
}

fn task_two(input: &[String]) -> usize {
    let mut map: HashMap<isize, Vec<(isize, isize)>> = HashMap::new();

    let mut curr: (isize, isize) = (0, 0);

    for line in input.iter() {
        let mut iter = line.split_ascii_whitespace();
        /* let hex = iter.last().unwrap();

        let num = &hex[2..7];
        let dir = &hex[7..8];

        let num = usize::from_str_radix(num, 16).unwrap();

        let (dy, dx) = match dir.trim() {
            "3" => (-1, 0),
            "1" => (1, 0),
            "0" => (0, 1),
            "2" => (0, -1),
            _ => unreachable!(),
        };
        println!("{} {}", dir, num);*/

        let dir = iter.next().unwrap();
        let num = iter.next().unwrap().parse::<usize>().unwrap();

        let (dy, dx) = match dir {
            "U" => (-1, 0),
            "D" => (1, 0),
            "R" => (0, 1),
            "L" => (0, -1),
            _ => unreachable!(),
        };

        for _ in 0..num {
            curr.0 += dy;
            curr.1 += dx;
            println!("{:?}", curr);
            map.entry(curr.0).or_default().push(curr);
        }
    }

    let mut sum = 0;
    let min = 0;
    let max: isize = *map.keys().max().unwrap();

    for cy in 0..=max {
        let mut vec = map.get_mut(&cy).unwrap();
        vec.sort();

        let mut local = 0;
        let mut start = Option::<isize>::None;
        let mut stop = Option::<isize>::None;

        for arr in vec.windows(2) {
            println!("{cy}: {:?}", arr);
            if start.is_none() {
                start = Some(arr[0].1);
            }
            if arr[1].1 != (arr[0].1 + 1) {
                stop = Some(arr[1].1);
            } else {
                // do something?
            }

            if let (Some(st), Some(ed)) = (start, stop) {
                local += ed - st;
                start = None;
                stop = None;
            }
        }
        if let (Some(st), None) = (start, stop) {
            local += vec.len() as isize;
        }
        println!("{}", local);
        sum += local;
    }

    sum as usize
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
